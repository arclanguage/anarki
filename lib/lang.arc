; originally from http://awwx.ws/lang2.arc

(require "lib/template.arc")
(require "lib/re.arc")
(require "lib/lock.arc")

(implicit lang*)

(= lcodetag* 'code-jfYQQpLieX)

(mac lcode body
  `(cons lcodetag* (tostring ,@body)))

(def is-lcode (x)
  (and (acons x) (is (car x) lcodetag*)))

(= lcodeof cdr)

(def prplbool (x)
  (pr (if x "1" "0")))

(def plbool (x)
  (lcode (prplbool x)))

(def prpybool (x)
  (pr (if x "True" "False")))

(def pybool (x)
  (lcode (prpybool x)))

(def lbool (x)
  ((case lang*
     perl   plbool
     python pybool)
   x))

(def lpad (n s (o p " "))
  (string (n-of (- n len.s) p) s))

(def 2hex (i)
  (if (< i 16) (writec #\0))
  (pr (coerce i 'string 16)))

(def 4hex (i)
  (pr (lpad 4 (coerce i 'string 16) "0")))

(def escape-python-string (s)
  (each c s
    (let i (int c)
      (if (> i 255)
           (do (pr "\\u") (4hex i))
          (or (< i 32) (> i 126) (in c #\\ #\'))
           (do (pr "\\x") (2hex i))
           (pr c)))))

(def prplstr (s)
  (pr #\')
  (pr (re-replace "[\\\\']" (string s) "\\\\&"))
  (pr #\'))

(def plstr (s)
  (lcode (prplstr s)))

(def prpystr (s)
  (pr #\') (escape-python-string (string s)) (pr #\'))

(def pystr (s)
  (lcode (prpystr s)))

(def prpyustr (s)
  (pr "u'") (escape-python-string (string s)) (pr "'"))

(def pyustr (s)
  (lcode (prpyustr s)))

(def lstr (x)
  ((case lang*
     perl   plstr
     python pyustr)
   x))

(def prpllist (lst)
  (pr #\[)
  (between x lst (pr ",") (prpl x))
  (pr #\]))

(def pllist (lst)
  (lcode (prpllist lst)))

(def prpylist (lst)
  (pr #\[)
  (between x lst (pr ",") (prpy x))
  (pr #\]))

(def pylist (lst)
  (lcode (prpylist lst)))

(def llist (lst)
  ((case lang*
     perl   pllist
     python pylist)
   lst))

(def prplhash (tab)
  (pr #\{)
  (between (k v) tab (pr ",")
    (prplstr k)
    (pr #\:)
    (prpl v))
  (pr #\}))

(def plhash (tab)
  (lcode (prplhash tab)))

(def prpydict (tab)
  (pr #\{)
  (between (k v) tab (pr ",")
    (prpy k)
    (pr ":")
    (prpy v))
  (pr #\}))

(def pydict (tab)
  (lcode (prpydict tab)))

(def lobj (x)
  ((case lang*
     perl   plhash
     python pydict)
   x))

(def prpl (x)
  (if (is-lcode x)             (pr (lcodeof x))
      (alist x)                (prpllist x)
      (in type.x 'string 'sym) (prplstr x)
      (in type.x 'int 'num)    (pr x)
      (isa x 'table)           (prplhash x)
      (err "Don't know how to convert to perl" x)))

(def prpy (x)
  (if (is-lcode x)             (pr (lcodeof x))
      (alist x)                (prpylist x)
      (in type.x 'string 'sym) (prpyustr (string x))
      (in type.x 'int 'num)    (pr x)
      (isa x 'table)           (prpydict x)
      (err "Don't know how to convert to python" x)))

(def tolang (x)
  ((case lang*
     perl   prpl
     python prpy)
   x))

(= lang-control-port* 50000)

(= lang-listen-port*
   (obj perl   50001
        python 50002))

(def open-lang-control-socket ()
  (thread
   (let s (open-socket lang-control-port*)
     (xloop ()
       (socket-accept s)
       (next)))))

(open-lang-control-socket)

(= lang-error-key* 'error_m3Sov8Qsk9)

(= lang-subprocess-code* (table)
   lang-server-code*     (table))


(mac w/lang (lang . body)
  `(w/lang* ,lang
     (w/guillemet tolang
       ,@body)))

(= perl2arc* (tostring (w/lang 'perl “
sub arcval {
    bless { val => $_[0] }, 'ArcVal';
}

sub encodearcstr {
    my ($x) = @_;

    $x =~ s!([\"\\])!\\\1!g;
    '"' . $x . '"';
}

sub arcstr {
    arcval(encodearcstr($_[0]));
}

sub arcnum {
    arcval($_[0]);
}

# TODO need to encode in some cases
sub arcsym {
    arcval($_[0]);
}

sub arcnil { arcval('nil') }
sub arct   { arcval('t') }

sub arcbool {
    $_[0] ? arct : arcnil;
}

sub toarclist {
    my ($aref) = @_;

    '(' . join(' ', map(toarc($_), @$aref)) . ')';
}

sub arclist {
    arcval(toarclist(\@_));
}

sub toarctab {
    my ($href) = @_;

    '{' .
    join(' ', map { toarc($_) . ' ' . toarc($href->{$_}) } keys %$href) .
    '}';
}

sub arcsymtab {
    my ($href) = @_;

    arcval('{' .
           join(' ', map { $_ . ' ' . toarc($href->{$_}) } keys %$href) .
           '}');
}

sub toarc {
    my ($x) = @_;

    my $type = ref($x);

    if (! $type) {
        return encodearcstr($x)
    }
    elsif ($type eq 'ArcVal') {
        return $x->{val};
    }
    elsif ($type eq 'ARRAY') {
        return toarclist($x);
    }
    elsif ($type eq 'HASH') {
        return toarctab($x);
    }
    else {
        return encodearcstr("$x");
    }
}

sub arc_encode_err {
    my ($bang) = @_;

    toarc(arcsymtab({ «lang-error-key*» => arct,
                      message => "$@" }));
}

sub eval2arc {
    my ($code) = @_;
    my $result;
    my $retval = eval $code;
    if ($@) {
        $result = arc_encode_err($@);
    }
    else {
        $result = eval { toarc($retval) };
        $result = arc_encode_err($@) if $@;
    }
    $result;
}
”)))


(= python2arc* (tostring (w/lang 'python “
from sys import exc_info, stderr
import traceback

class arcval:
    def __init__(self, x):
        self.v = x

arcnil = arcval("nil")
arct   = arcval("t")

def arcbool(x):
    if x:
        return arct
    else:
        return arcnil

# TODO don't encode chars unnecessarily

def strarcstr(x):
    r = '"'
    for c in x:
        r += "\\x%02x" % ord(c)
    r += '"'
    return r

# TODO unicode chars outside of the Basic Multilingual Plane

def ustrarcstr(x):
    r = '"'
    for c in x:
        r += "\\u%04x" % ord(c)
    r += '"'
    return r

def arcnum(x):
    return arcval(str(x))

def arcstr(x):
    if isinstance(x, str):
        return arcval(strarcstr(x))
    elif isinstance(x, unicode):
        return arcval(ustrarcstr(x))
    else:
        return arcval(strarcstr(repr(x)))

# TODO need to encode in some cases
def toarcsym(x): return str(x)

def arcsym(x):
    return arcval(toarcsym(x))

def arclist(x):
    return arcval(toarclist(x))

def toarclist(x):
    return "(" + " ".join(map(toarc, x)) + ")"

def toarctab(x):
    return ("{" +
            " ".join(map(lambda (k, v): toarc(k) + " " + toarc(v),
                         x.items())) +
            "}")

def arcsymtab(x):
    return arcval("{" +
                  " ".join(map(lambda (k, v): toarcsym(k) + " " + toarc(v),
                               x.items())) +
                  "}")

def toarc(x):
    if isinstance(x, arcval):
        return x.v
    elif (x is None or
          x is False):
        return "nil"
    elif x is True:
        return "t"
    elif isinstance(x, str):
          return strarcstr(x)
    elif isinstance(x, unicode):
        return ustrarcstr(x)
    elif (isinstance(x, int) or
          isinstance(x, long) or
          isinstance(x, float)):
        return str(x)
    elif (isinstance(x, tuple) or
          isinstance(x, list)):
        return toarclist(x)
    elif isinstance(x, dict):
        return toarctab(x)
    else:
        return toarc(repr(x))

# TODO message is working for some errors like division by zero but not
# others like syntax error

def arc_encode_traceback():
    r = {«(pystr lang-error-key*)»: True,
         'message': ''.join(traceback.format_exception_only(exc_info()[0], exc_info()[1])),
         'traceback': traceback.format_exc()}
    return toarc(arcsymtab(r))

# Using a global variable for the return value is ugly, but was the only
# way I could find to get "import foo" to work and also to be able to
# return a value at the same time.  It's safe as long as only one thread
# ever calls eval2arc within a process.

result = None

def eval2arc (code):
    global result
    result = None
    try:
        exec code in globals()
    except Exception:
        return arc_encode_traceback()

    try:
        return toarc(result)
    except Exception:
        return arc_encode_traceback()
”)))


(= lang-subprocess-code*!perl
   (fn (program)
     (w/lang 'perl
       “
use strict;
”
       (pr perl2arc*)
       “
print eval2arc(«program»), "\n";
”)))


(= lang-subprocess-code*!python
   (fn (program)
     (w/lang 'python
       (pr python2arc*)
       “
print eval2arc(«program»)
”)))


(= lang-server-code*!perl
   (tostring
    (w/lang 'perl “
use strict;
use AnyEvent::HTTPD;
require POSIX;
”
      (pr perl2arc*)
      “
fork && exit;
# open STDIN, '</dev/null' or die "Can't open STDIN from /dev/null: $!\n";
# open STDOUT, '>/dev/null' or die "Can't open STDOUT on /dev/null: $!\n";

my $handle; $handle = new AnyEvent::Handle
  connect => ['127.0.0.1', «lang-control-port*»],
  on_error => sub {
    print STDERR "$!\n";
    exit 1;
  },
  on_eof => sub {
    exit;
  },
  on_read => sub {};

my $httpd = AnyEvent::HTTPD->new(
  host => '127.0.0.1',
  port => «lang-listen-port*!perl»);

$httpd->reg_cb (
  '/' => sub {
    my ($httpd, $req) = @_;

    $req->respond({ content => ['text/plain; charset=utf-8',
                                eval2arc($req->parm('code'))] });
  });

AnyEvent->condvar->wait;
”)))


(= lang-server-code*!python
   (tostring
    (w/lang 'python “
import os
import re
from twisted.web import server, resource
from twisted.internet import protocol, reactor
”
      (pr python2arc*)
      “
class Eval(resource.Resource):
    isLeaf = True

    def render_GET(self, request):
        return self.render_POST(request)

    def render_POST(self, request):
        request.setHeader('Content-Type', 'text/plain')
        return eval2arc(request.args['code'][0])

def stop():
    try:
        reactor.stop()
    except:
        pass

class Control(protocol.Protocol):
    def connectionLost(self, reason):
        stop()

class ControlClientFactory(protocol.ClientFactory):
    def buildProtocol(self, addr):
        return Control()

    def clientConnectionFailed(self, connector, reason):
        stop()

if os.fork():
    os._exit(0)

dev_null = os.open('/dev/null', os.O_RDWR)
os.dup2(dev_null, 0)
os.dup2(dev_null, 1)

site = server.Site(Eval())
reactor.connectTCP("localhost", «lang-control-port*», ControlClientFactory())
reactor.listenTCP(«lang-listen-port*!python», site, interface='127.0.0.1')
reactor.run()
”)))

(= lang-command*
   (obj perl   "perl"
        python "python"))

(= launch-locks* (obj perl (lock) python (lock)))
(= lang-launched* (obj))

(= connection-refused* 'connection-refused-flWdhv41PP)

(def postprog (lang program)
  (on-err
   (fn (c)
     (if (re-match-pat "Connection refused" (details c))
          connection-refused*
          (err details.c)))
   (fn ()
     (postform (string "http://localhost:" lang-listen-port*.lang "/")
               `((code ,program))
               (fn (r i) (read i))))))

(def launch-lang (lang)
  (atomic-lock launch-locks*.lang
    (unless lang-launched*.lang
      (fromstring lang-server-code*.lang (system lang-command*.lang))
      (xloop (count 0)
        (when (> count 20)
          (err (string lang " server did not start")))
        (when (is (postprog lang nil) connection-refused*)
          (sleep .1)
          (next (+ count 1))))
      (set lang-launched*.lang))))

(def leftpad (i x)
 (string (n-of (- i (len string.x)) " ") x))

(def print-program (program)
 (fromstring program
  (let linenumber 1
   (whilet line (readline)
     (disp (string (leftpad 3 linenumber) ": " line "\n") (stderr))
     (++ linenumber)))))

(mac langex (lang . body)
  `(do (w/lang ',lang ,@body)
       (prn)))

(def lang-check-error (lang program result)
  (when (and (isa result 'table) (result lang-error-key*))
    (disp (string "\n" lang " error:\n") (stderr))
    (disp (or result!traceback result!message) (stderr))
    (print-program program)
    (disp "\n" (stderr))
    (err (string lang " error: " result!message)))
  result)

(def subprocess (lang program)
  (lang-check-error lang program
    (let s (fromstring (tostring ((lang-subprocess-code* lang) program))
             (pipe-from lang-command*.lang))
      (after (read s) (close s)))))

(def singlethread (lang program)
  (launch-lang lang)
  (lang-check-error lang program (postprog lang program)))

(mac expr code
  `(do (pr "result = (")
       ,@code
       (pr ")\n")))

(mac lang (lang runtime . args)
  `(w/lang ',lang
     (,runtime ',lang (tostring ,@args))))

(mac perl args
  `(lang perl ,@args))

(mac python args
  `(lang python ,@args))

