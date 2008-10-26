; Main Arc lib.  Ported to Scheme version Jul 06.
; Rewritten Oct 08 for Arc-F

; optimize ~foo in functional position in ac, like compose
; make foo~bar equiv of foo:~bar (in expand-ssyntax)
; rename assert
; (10 x) for (= x 10)?
; should (= x)  mean (= x t)?
; add sigs of ops defined in ac.scm
; get hold of error types within arc
; why is macex defined in scheme instead of using def below?
; write disp, read, write in arc
; could prob write rmfile and dir in terms of system
; could I get all of macros up into arc.arc?
; warn when shadow a global name
; permanent objs that live on disk and are updated when modified
;  problem: serialization of shared structure, especially cycles
;  (need eq?-tables!!)
; way to spec default 0 rather than nil for hts
;  do in access call or when ht created?  simply have ++ nil -> 1?
; some simple regexp/parsing plan

; compromises in this implementation: 
; no objs in code
;  (mac testlit args (listtab args)) breaks when called
; separate string type
;  (= (cdr (cdr str)) "foo") couldn't work because no way to get str tail

(in-package arc)

; NOTE!  Do not modify.  We can't modify this
; without potentially breaking existing code.
; Instead use <arc>v3-exp for new stuff.
; If it's good, it'll be added into <arc>v4

;-----DO NOT MODIFY BEGINS

; separated, because not all Arc implementations
; might support threads (or, like SNAP, may
; support an incompatible sort of concurrency)
(interface <arc>v3-thread
  thread new-thread kill-thread break-thread
  dead)
; similar: might not be supported by all Arc
; implementations
(interface <arc>v3-sync
  pipe pipe-len thread-local
  sema sema-wait sema-post
  sync synct
  lock w/lock)
; almkglor: controversial? LOL
(interface <arc>v3-reductor
  l-reductor t-reductor r-reductor)
(interface <arc>v3-polymorph
  polymorph)
; potentially rarely useful
(interface <arc>v3-testify
  testify)
; almkglor: controversial? LOL
(interface <arc>v3-scanner
  scanner unscan
  each each-skip each-early-out each-skip-early-out
  w/collect)
; all sorts of stuff
(interface <arc>v3-sort-internals
  insert-sorted insort reinsert-sorted insortnew
  mergesort)
; alternative multimethods
(interface <arc>v3-dispatcher-method
  dispatcher-method add-dispatcher-method)
; almkglor: in my opinion, $ should be
; reserved for implementation-specific
; "hooks"
(interface <arc>v3-on-mzscheme
  $)
; because you sometimes need to
; differentiate between what would
; have been there, versus what is
; really there
(interface <arc>v3-IDEAL
  IDEAL REAL)
; to gain access to hidden help stuff
(interface <arc>v3-help
  help helpsearch helpstr fns)
; packages
(interface <arc>v3-packages
  cxt pkg pkg-of pkg-name unpkg
  cxt-ref-d)
(interface <arc>v3
  ; types
  bool cons sym fn char string
  int num table output input
  socket exception re thread
  thread-local sema
  ; maths
  + - * / mod quotient expt sqrt
  trunc exact
  abs signop positive
  round roundup to-nearest
  avg
  multiple
  ; range
  range
  ; comparison
  is isnt < > <= >=
  iso
  in empty
  caris prefix mismatch
  compare
  single
  len< len>
  ; character classification punc
  whitec nonwhite alphadig
  ; running code
  apply eval
  ; higher-order functions
  compose complement >> andf orf
  memo
  par
  only
  ; definitional structures
  def mac defm defcall
  defmemo
  redef
  ; code grouping
  do do1 let with withs given givens
  ; cons creation and manipulation
  cons list
  makeproper
  ; string creation and manipulation
  newstring tostring fromstring
  ; string playing
  ellipsize
  ; table creation and manipulation
  table maptable fill-table
  w/table memtable
  keys vals
  ; table read and write
  load-table read-table
  load-tables save-table
  write-table
  safe-load-table
  ; expression reading
  readstring1 read readfile readfile1
  readall
  saferead
  ; scanner traversal
  car cdr caar cadr cdar cadr
  carif
  nthcdr last
  each
  reclist
  noisy-each
  some  all     ;note the equivalency of these
  ormap andmap
  mem find
  ; mutation
  scar scdr sref
  ; assignment
  = defset
  push swap rotate pop
  pushnew pull
  ++ --
  zap
  wipe assert
  ; control structures
  and or nor
  when unless
  while until
  loop for repeat n-of
  forlen on
  case
  whilet caselet iflet whenlet
  drain whiler
  check
  ; simple functions
  no idfn nilfn
  even odd
  ; user types
  annotate rep type
  ; type checks
  ; - should be less useful given generics
  ; - also least-consistently-named part of ArcN
  isa acons atom alist number dotted
  ; other traversal structures
  recstring trav trav+
  ontable
  ; collecting structures
  w/collect w/scanner-collect-each
  w/collect-on
  accum accums
  summing
  intersperse
  counts
  ; filtering and choosing
  keep rem trues
  best max min
  most least
  bestn firstn-that
  union
  count
  before
  dedup
  commonest
  ; scanner/sequence manipulation
  map1 map mappend pair tuples mapeach
  butlast cut firstn
  split ; list only
  ssplit ; string only
  join
  rev len
  at ; list and string only
  adjoin consif conswhen ; list only
  flat
  copy-seq seq-to-list
  sort
  merge ; list only
  atend ; len-able sequence only
  ; scanner/sequence copying
  copy-seq seq-to-list
  ; scanner reduction
  reduce rreduce
  ; cons cells as trees
  tree-subst treewise ontree
  ; tables as objects
  obj inst deftem addtem
  templatize
  temread temload temloadall
  tems
  ; queues
  queue enq deq qlen qlist enq-limit
  ; hooks
  hook defhook
  ; association lists
  assoc alref
  tablist listtab
  ; bracket functions
  ; - needed if say some macro syntax would appreciate []
  make-br-fn _
  ; anaphoric functions
  rfn afn
  ; anaphoric structures
  awhile aif aand awhen
  ; anaphora
  self it break collect throw
  ; continuations and weirder control structures
  ccc breakable point
  protect dynamic-wind
  after
  ; gensyms
  uniq w/uniq
  ; printing
  disp write pr prn
  prall prs
  prf
  warn ero
  writefile1
  writefileraw
  bar* ; global variable, not very good T.T
  w/bars
  ; error
  err on-err details
  errsafe
  ; i/o
  infile   outfile   instring   outstring
  w/infile w/outfile w/instring w/outstring w/appendfile append
  inside
  close
  stdout stdin sterr
  call-w/stdout call-w/stdin
  w/stdout      w/stdin
  readc writec peekc
  readb writeb
  readline
  ; sockets
  open-socket socket-accept
  client-ip
  connect-socket flush-socket
  w/socket
  ; conversion
  string sym coerce
  downcase upcase
  ; GIL operations
  atomic-invoke atomic atlet atwith atwiths
  ; random
  rand-string rand rand-choice random-elt
  ; filesystem and system
  dir file-exists dir-exists
  rmfile
  mkdir ensure-dir
  system pipe-from
  which-os
  ; time
  sleep msec seconds
  datetbl
  current-process-milliseconds
  current-gc-milliseconds
  since minutes-since hours-since
  days-since
  date
  ; timed cache
  cache
  ; profiling
  time jtime time10
  ; global variable queries
  bound varif
  ; PG wierdness
  copy
  ; loading sources
  load require arc-installation-path
  ; getting out of a lousy fork of Arc
  quit
  ; macro playing
  expand macex macex1
  ; help facilities
  help fns
  docstring)

;-----DO NOT MODIFY ENDS

; NOTE! add new structures and functions here, *not*
; in any of the above interfaces.  Users of this
; interface are warned that it could break their
; code at any random time.
; if you want to propose a new interface for <arc>v4,
; provide it first in the form <arc>v3-your-interface-exp
; :s/your-interface/whatever you want/
; i.e. with the -exp tag to denote its experimental
; status for 3F
(interface <arc>v3-bitops-exp
  bit-and bit-or bit-not bit-xor bit-shift)
(interface <arc>v3-your-interface-exp)

(interface <arc>v3-exp
  <arc>v3
  <arc>v3-bitops-exp
  ; mathematics
  log
  ; sequence checking
  pos
  ; number representation
  pad
  ; control structures
  catch always)

; NOTE! THIS INTERFACE EXISTS ONLY FOR DOCUMENTATION PURPOSES
; IT IS NOT INTENDED FOR ACTUAL USAGE
(interface <base>v3
  ; maths
  <base>+ <base>- <base>negate <base>*
  <base>/ <base>reciprocal <base>mod
  <base>quotient
  ; comparison
  <base>is <base><
  ; higher-order functions
  <base>andf <base>orf
  ; scanner efficiency overrides
  <base>each <base>collect-on)

(set help* (table))

(set current-load-file* "arc.arc")
(set source-file* (table))

(set safeset (annotate 'mac
               (fn (var val)
                 `((fn ()
                     (if (bound ',var)
                         ((fn ()
                            (disp "*** redefining ")
                            (disp ',var)
                            (writec #\newline))))
                     (set ,var ,val))))))

(set mac (annotate 'mac
           (fn (name parms . body)
             `((fn ()
                 (sref sig ',parms ',name)
                 ; Document the macro, including the docstring if present
                 (if (<base>is (type ',(car body)) 'string)
                     (sref help* '(mac ,(car body)) ',name)
                     (sref help* '(mac nil) ',name))
                 (sref source-file* current-load-file* ',name)
                 (safeset ,name (annotate 'mac (fn ,parms ,@body))))))))
;documentation for mac itself
(sref help*
  '(mac
  " Defines a macro, a special function which transforms code.
    You may specify a docstring to be displayed by `help' by
    placing it as the first string in `body'
    See also [[def]] [[docstring]] ")
  'mac)
(sref sig
  '(name parms . body)
  'mac)
(sref source-file*
  current-load-file*
  'mac)

(mac do args
  " Evaluates each expression in sequence and returns the result of the
    last expression.
    See also [[do1]] [[after]] "
  `((fn () ,@args)))

(mac IDEAL (ideal real-tag real)
  " Used to differentiate between a form that
    we would prefer to use ideally, versus a
    form that exists solely for real-world
    efficiency. "
  (if (<base>is real-tag 'REAL)
      ()
      (err "syntax error in IDEAL form"))
  real)

; It would be nice if multiple strings counted as multiple docstring lines.
(mac def (name parms . body)
  " Defines a function with the given `name', `parms', and `body'.
    You may specify a docstring to be displayed by `help' by
    placing it as the first string in `body'
    See also [[fn]] [[mac]] [[defm]] [[docstring]] "
  `(do (sref sig ',parms ',name)
       ; Document the function, including the docstring if present
       ,(if (<base>is (type (car body)) 'string)
           `(sref help* '(fn ,(car body)) ',name)
           `(sref help* '(fn nil) ',name))
       (sref source-file* current-load-file* ',name)
       (safeset ,name (fn ,parms ,@body))))

(def docstring (name type parms docstring)
  " A function which creates a docstring for
    a given `name' symbol.  `type' and `parms'
    are the type of the `name' and the
    parameters to that name.  `docstring' is
    the actual docstring for use with `help'.
    See also [[help]] [[def]] [[mac]] "
  (sref help* `(,type ,docstring) name)
  (sref sig parms name)
  (sref source-file* current-load-file* name)
  t)

(mac let (var val . body)
  " Assigns a local variable for the given `body'.
    See also [[with]] [[withs]] [[fn]] [[do]] "
  `((fn (,var)
      ,@body)
    ,val))

(def caar (xs) " Equivalent to (car (car xs)) " (car (car xs)))
(def cadr (xs) " Equivalent to (car (cdr xs)) " (car (cdr xs)))
(def cdar (xs) " Equivalent to (cdr (car xs)) " (cdr (car xs)))
(def cddr (xs) " Equivalent to (cdr (cdr xs)) " (cdr (cdr xs)))

(def no (x) " Determines if `x' is `nil'. " (<base>is x nil))

(def acons (x)
  " Determines if `x' is a `cons' cell or list.
    Unlike 'alist, this function will return nil if given an empty list
    See also [[atom]] [[alist]] [[dotted]] [[isa]] [[cons]] [[list]] "
  (<base>is (type x) 'cons))

(def atom (x)
  " Determines if `x' is an atom
    See also [[acons]] [[isa]] "
  (no (acons x)))

(def list args
  " Creates a list from the given parameters.
    See also [[cons]] [[acons]] "
  args)

(def idfn (x)
  " Identity function - just returns its argument.
    See also [[nilfn]] "
  x)

(def nilfn args
  " Takes any number of arguments and returns nil.
    See also [[idfn]] "
  nil)

; define reductors
(IDEAL
 (do ; ideally, this is the definition for l-reductor
     ; however, due to speed issues, l-reductors have
     ; been made into special types internally on
     ; the mzscheme side
     ; other implementations are free to use this
     ; definition, or to imitate this implementation
     ; and create special objects for l-reductors
     ; (provided that they are indistinguishable
     ; by Arc from true functions)
     (def l-reductor (f0 f1 f2)
       " Creates a function which accepts any number of
         parameters.  If the function is given no
         parameters, f0 is executed, if given 1 parameter
         f1 is executed, if given 2 parameters f2 is
         executed.  For more than 2 parameters, parameters
         are paired left-to-right:
           (= f (l-reductor f0 f1 f2))
           (f)        ==  (f0)
           (f a)      ==  (f1 a)
           (f a b)    ==  (f2 a b)
           (f a b c)  ==  (f2 (f2 a b) c)
         See also [[r-reductor]] [[t-reductor]] "
       (fn rest
         (if (no rest)         (f0)
             (no:cdr rest)     (f1:car rest)
             (no:cdr:cdr rest) (f2 (car rest) (cadr rest))
           (l-reduction f2 (f2 (car rest) (cadr rest)) (cdr:cdr rest)))))
     ; this function is *not* part of the <arc>v3
     ; interface, or any interface; portable arc
     ; programs should not rely on its presence
     ; *or* absence.
     ; This also means that other implementations
     ; are free to include various internal
     ; functions in arc.arc, provided they are not 
     ; included in a standard interface
     (def l-reduction (f acc args)
       (if args
           (l-reduction f (f acc (car args)) (cdr args))
           acc)))
 REAL
 ; l-reductor is defined scheme-side
 (docstring 'l-reductor 'fn '(f0 f1 f2)
       " Creates a function which accepts any number of
         parameters.  If the function is given no
         parameters, f0 is executed, if given 1 parameter
         f1 is executed, if given 2 parameters f2 is
         executed.  For more than 2 parameters, parameters
         are paired left-to-right.
           (= f (l-reductor f0 f1 f2))
           (f)        ==  (f0)
           (f a)      ==  (f1 a)
           (f a b)    ==  (f2 a b)
           (f a b c)  ==  (f2 (f2 a b) c)
         See also [[r-reductor]] [[t-reductor]] "))

; use wrapper functions to allow overriding of
; basic math functions
(set + (l-reductor
         (fn ()    (err "'+ requires at least one parameter"))
         (fn (x)   x)
         (fn (a b) (<base>+ a b))))
(docstring '+ 'fn 'args
  " Adds numbers together, or concatenates
    strings and lists.
    See also [[-]] [[*]] [[/]] [[join]] ")

(set - (l-reductor
         (fn ()    (err "'- requires at least one parameter"))
         (fn (x)   (<base>negate x))
         (fn (a b) (<base>- a b))))
(docstring '- 'fn 'args
  " Subtracts other numbers from the first given
    number, or negates a single number.
      (- 10 5 4)  =>  (10 - 5) - 4 == 1
      (- 10)      =>  -10
    See also [[-]] [[*]] [[/]] ")

(set * (l-reductor
         (fn ()    (err "'* requires at least one parameter"))
         (fn (x)   x)
         (fn (a b) (<base>* a b))))
(docstring '* 'fn 'args
  " Multiplies numbers together.
    See also [[+]] [[-]] [[/]] ")

(set / (l-reductor
         (fn ()    (err "'/ requires at least one parameter"))
         (fn (x)   (<base>reciprocal x))
         (fn (a b) (<base>/ a b))))
(docstring '/ 'fn 'args
  " Divides a number by succeeding numbers,
    or takes the reciprocal of a number.
      (/ 60 3 2)  =>  (60 / 3) / 2 == 10
      (/ 0.25)    =>  4.0
    See also [[+]] [[-]] [[*]] [[mod]] [[quotient]] ")

(set mod
       (l-reductor
         (fn ()    (err "'mod requires at least two parameters"))
         (fn (x)   (err "'mod requires at least two parameters"))
         (fn (a b) (<base>mod a b))))
(docstring 'mod 'fn 'args
  " Divides a number by another number,
    returning the remainder (or
    'modulo'); the result is further
    divided and the remainder computed
    if more than two parameters are
    given.  All numbers must be integers.
      (mod 59 10)     => 9
      (mod 59 10 4)   =>  (59 mod 10) mod 4
                      =>  9 mod 4
                      =>  1
    See also [[quotient]] [[/]] ")

(set quotient
       (l-reductor
         (fn ()    (err "'quotient requires at least two parameters"))
         (fn (x)   (err "'quotient requires at least two parameters"))
         (fn (a b) (<base>quotient a b))))
(docstring 'quotient 'fn 'args
  " Divides a number by another number,
    returning the quotient, rounded
    towards zero to the nearest integer;
    the result is further divided and
    the quotient rounded if more than
    two parameters are given.  All
    numbers must be integers.
      (quotient 42 8)    => 5
      (quotient 42 8 2)  => round(round(42 / 8) / 2)
                         => round(5 / 2)
                         => 2
    See also [[mod]] [[/]] ")

(set compose
       (l-reductor
         (fn ()    (err "'compose requires at least one parameter"))
         (fn (x)   x)
         (fn (a b) (<base>compose a b))))
(docstring 'compose 'fn 'args
  " Connects several functions so that the
    result of the rightmost function is
    given to the function to the left,
    and so on.
      ((compose a b) x)  =>  (a (b x))
    See also [[>>]] [[complement]] ")

(IDEAL
 (def <base>compose (a b)
   (fn rest
     (a (apply b rest))))
 REAL
 ())

(def complement (f)
  " Arc expands ~x into (complement x)
    whenever the function returns true this returns false.
    See also [[no]] [[isnt]] [[compose]]"
  (<base>compose no f))

(IDEAL
 (do ; like l-reductor, this is handled
     ; by the base ac.scm for speed
     (def r-reductor (f0 f1 f2)
       " Creates a function which accepts any number of
         parameters.  If the function is given no
         parameters, f0 is executed, if given 1 parameter
         f1 is executed, if given 2 parameters f2 is
         executed.  For more than 2 parameters, parameters
         are paired right-to-left:
           (= f (r-reductor f0 f1 f2))
           (f)        ==  (f0)
           (f a)      ==  (f1 a)
           (f a b)    ==  (f2 a b)
           (f a b c)  ==  (f2 a (f2 b c))
         See also [[l-reductor]] [[t-reductor]] "
       (fn rest
         (if (no rest)         (f0)
             (no:cdr rest)     (f1:car args)
             (no:cdr:cdr rest) (f2 (car args) (cadr args))
           (f2 (car rest) (r-reduction f2 (cdr rest))))))
     (def r-reduction (f2 rest)
       (if (no:cdr rest)
           (car rest)
           (f2 (car rest) (r-reduction f2 (cdr rest))))))
 REAL
 (docstring 'r-reductor 'fn '(f0 f1 f2)
       " Creates a function which accepts any number of
         parameters.  If the function is given no
         parameters, f0 is executed, if given 1 parameter
         f1 is executed, if given 2 parameters f2 is
         executed.  For more than 2 parameters, parameters
         are paired right-to-left:
           (= f (r-reductor f0 f1 f2))
           (f)        ==  (f0)
           (f a)      ==  (f1 a)
           (f a b)    ==  (f2 a b)
           (f a b c)  ==  (f2 a (f2 b c))
         See also [[l-reductor]] [[t-reductor]] "))

; reduce right so that pairing is thus:
; (<base>andf a (<base>andf b c))
; this is slightly more efficient in the
; case of a function returning nil early
(set andf
       (r-reductor
         (fn ()    (fn (x) t))
         (fn (a)   (fn (x) (a x)))
         (fn (a b) (<base>andf a b))))
(docstring 'andf 'fn 'fns
  " Creates a function which returns true on its argument if all of the
    given `fns' return true on that argument.
    The created function accepts a single argument.
    See also [[and]] [[orf]] ")

(def <base>andf (a b)
  (fn (x)
    (if (a x) (b x))))

(set orf
       (r-reductor
         (fn ()    (fn (x) t))
         (fn (a)   (fn (x) (a x)))
         (fn (a b) (<base>orf a b))))
(docstring 'orf 'fn 'fns
  " Creates a function which returns true on its argument if any of the
    given `fns' return true on that argument.
    The created function accepts a single argument.
    See also [[or]] [[andf]] ")

(def <base>orf (a b)
  (fn (x)
    ((fn (rv)
       (if rv rv
              (b x)))
     (a x))))

(IDEAL
 (do ; efficiency concern, efficiency concern
     (def t-reductor (f0 f1 f2)
       " Creates a function which accepts any number of
         parameters.  If the function is given no
         parameters, f0 is executed, if given 1 parameter
         f1 is executed, if given 2 parameters f2 is
         executed.  For more than 2 parameters, parameters
         are paired left-to-right, and execution continues
         only if f2 on the previous pair returns true:
           (= f (t-reductor f0 f1 f2))
           (f)        ==  (f0)
           (f a)      ==  (f1 a)
           (f a b)    ==  (f2 a b)
           (f a b c)  ==  (if (f2 a b) (f2 b c))
         See also [[l-reductor]] [[r-reductor]] "
       (fn rest
         (if (no rest)         (f0)
             (no:cdr rest)     (f1:car rest)
             (no:cdr:cdr rest) (f2 (car rest) (cadr rest))
                                                       ; notice only cdr
           (t-reduction f2 (f2 (car rest) (cadr rest)) (cdr rest)))))
     (def t-reduction (f2 result rest)
       (if result
           (if (cdr:cdr rest)
               (t-reduction f2 (f2 (car rest) (cadr rest)) (cdr rest))
               (f2 (car rest) (cadr rest))))))
 REAL
 (docstring 't-reductor 'fn '(f0 f1 f2)
       " Creates a function which accepts any number of
         parameters.  If the function is given no
         parameters, f0 is executed, if given 1 parameter
         f1 is executed, if given 2 parameters f2 is
         executed.  For more than 2 parameters, parameters
         are paired left-to-right, and execution continues
         only if f2 on the previous pair returns true:
           (= f (t-reductor f0 f1 f2))
           (f)        ==  (f0)
           (f a)      ==  (f1 a)
           (f a b)    ==  (f2 a b)
           (f a b c)  ==  (if (f2 a b) (f2 b c))
         See also [[l-reductor]] [[r-reductor]] "))

(set is
     (t-reductor
       (fn ()    (err "'is expects at least one argument"))
       (fn (a)   (fn (b) (is a b)))
       (fn (a b) (<base>is a b))))
(docstring 'is 'fn 'args
  " Determines if all arguments are
    equal.
    If given only one argument, returns
    a function that compares its
    argument to that value.
      (is 1 2)   => nil
      (is 1 1)   => t
    (= f (is 0))
      (f 2)      => nil
      (f 0)      => t
    See also [[isnt]] [[iso]] [[<]] [[>]] [[<=]] [[>=]] ")

(set isnt
     (t-reductor
       (fn ()    (err "'isnt expects at least one argument"))
       (fn (a)   (fn (b) (isnt a b)))
       (fn (a b) (no (<base>is a b)))))
(docstring 'isnt 'fn 'args
  " Determines if all arguments are not
    equal.
    If given only one argument, returns
    a function that compares its
    argument to that value.
      (isnt 1 2)   => t
      (isnt 1 1)   => nil
    (= f (isnt 0))
      (f 2)      => t
      (f 0)      => nil
    See also [[no]] [[is]] [[<]] [[>]] [[<=]] [[>=]] ")

(set <
     (t-reductor
       (fn ()    (err "'< expects at least one argument"))
       (fn (a)   (fn (b) (< a b)))
       (fn (a b) (<base>< a b))))
(docstring '< 'fn 'args
  " Determines if arguments are in order
    from least to highest.
    If given only one argument, returns
    a function that compares its
    argument to that value.
      (< 1 2)    => t
      (< 1 1)    => nil
      (< 1 2 3)  => t
      (< 1 3 2)  => nil
    (= f (< 0))
      (f 1)      => t
      (f -1)     => nil
    See also [[<=]] [[>]] [[>=]] [[is]] [[isnt]] ")

(set >
     (t-reductor
       (fn ()    (err "'> expects at least one argument"))
       (fn (a)   (fn (b) (> a b)))
       (fn (a b) (if (<base>< a b)
                     nil
                     (no:<base>is a b)))))
(docstring '> 'fn 'args
  " Determines if arguments are in order
    from highest to lowest.
    If given only one argument, returns
    a function that compares its
    argument to that value.
      (> 1 2)    => nil
      (> 1 1)    => nil
      (> 1 2 3)  => nil
      (> 3 2 1)  => t
    (= f (> 0))
      (f 1)      => nil
      (f -1)     => t
    See also [[>=]] [[<=]] [[<]] [[is]] [[isnt]] ")


(set <=
     (t-reductor
       (fn ()    (err "'<= expects at least one argument"))
       (fn (a)   (fn (b) (<= a b)))
       (fn (a b) ((fn (rv)
                    (if rv
                        rv
                        (<base>is a b)))
                  (<base>< a b)))))
(docstring '<= 'fn 'args
  " Determines if arguments are not
    decreasing.
    If given only one argument, returns
    a function that compares its
    argument to that value.
      (<= 1 2)    => t
      (<= 3 2)    => nil
      (<= 1 1 2)  => t
      (<= 3 3 2)  => nil
    (= f (<= 0))
      (f 1)      => t
      (f -1)     => nil
    See also [[<]] [[>=]] [[>]] [[is]] [[isnt]] ")

(set >=
     (t-reductor
       (fn ()    (err "'>= expect at least one argument"))
       (fn (a)   (fn (b) (>= a b)))
       (fn (a b) (if (<base>< a b)
                     (<base>is a b)
                     t))))
(docstring '>= 'fn 'args
  " Determines if arguments are not
    increasing.
    If given only one argument, returns
    a function that compares its
    argument to that value.
      (>= 1 2)    => nil
      (>= 3 2)    => t
      (>= 1 1 2)  => nil
      (>= 3 3 2)  => t
    (= f (>= 0))
      (f 1)      => nil
      (f -1)     => t
    See also [[>]] [[<=]] [[<]] [[is]] [[isnt]] ")

(mac and args
  " Evaluates arguments till false is found else returns the last one.
    See also [[or]] [[aand]] [[andf]] [[andmap]] "
  (if args
      (if (cdr args)
          `(if ,(car args) (and ,@(cdr args)))
          (car args))
      't))

(mac defm (name parms . body)
  " Defines or redefines a method, with type
    annotations.  A generic function must be
    defined with 'def before using this form.
    Example:
      (defm sref ((t c container) value)
        (scar (rep c) value))
    See also [[def]] [[defcall]]"
  (let (real-parms parmstl
        types typestl
        self) nil
    ; withs not defined yet ^^
    (let add-parm 
         (fn (p)
           (if real-parms
               (set parmstl    (scdr parmstl  (list p)))
               (set real-parms (set  parmstl  (list p)))))
      (let add-type
           (fn (tt)
             (if types
                 (set typestl (scdr typestl  (list tt)))
                 (set types   (set  typestl  (list tt)))))
        (let rest-parm
             (fn (p)
               (if real-parms
                   (scdr parmstl p)
                   (set real-parms p)))
          (set self
               (fn (l)
                 (if (is (type l) 'cons)
                     (let parm (car l)
                        (if (and (is (type parm) 'cons)
                                 (is (car parm) t))
                            (let (spec real-parm typ) parm
                              (add-parm real-parm)
                              (add-type typ))
                            (do (add-parm parm)
                                (add-type nil)))
                       (self:cdr l))
                     (rest-parm l))))
          (self parms)
          `(set ,name
             (symeval!polymorph
               ',types (fn ,real-parms ,@body)
               ,name)))))))

; bootstrap ends

(mac w/collect body
  " Creates a new list, providing a `collect' function
    within `body' which collects its argument into the
    list.
    Collection is in the correct order.
    See also [[accum]] [[w/collect-on]] [[summing]] "
  `(symeval!w/collect-f (fn (collect) ,@body)))
(def w/collect-f (bf)
  (let (hd tl collect) nil
     (set collect
          (fn (e)
            (if hd  (set tl (scdr tl (cons e nil)))
                    (set tl (set  hd (cons e nil))))))
     (bf collect)
     hd))

; defined here, but <base>each is defined later
(mac each (var expr . body)
  " Performs `body' for each element of the sequence returned by `expr',
    with each element assigned to `var'.
    See also [[forlen]] [[on]] [[map]] [[mapeach]] [[ontable]]
    [[each-skip-early-out]] "
  `(symeval!<base>each ,expr 0 (fn (,var) ,@body t)))

(mac each-skip-early-out (start var expr . body)
  " Performs `body' for each element of the sequence returned by `expr',
    starting at `start'.  If the last expression in `body' returns nil,
    ends iteration.
    See also [[each]] [[each-skip]] [[each-early-out]] "
  `(symeval!<base>each ,expr ,start (fn (,var) ,@body)))

(mac each-skip (start var expr . body)
  " Performs `body' for each element of the sequence returned by `expr',
    starting at `start'.
    See also [[each]] [[each-skip-early-out]] [[each-early-out]] "
  `(symeval!<base>each ,expr ,start (fn (,var) ,@body t)))

(mac each-early-out (var expr . body)
  " Performs `body' for each element of the sequence returend by `expr'.
    If the last epression in `body' returns nil, ends iteration,
    See also [[each]] [[each-skip]] [[each-skip-early-out]] "
  `(symeval!<base>each ,expr 0 (fn (,var) ,@body)))

(mac w/collect-on (seq . body)
  " Constructs a new sequence with the same type as `seq',
    and provides a `collect' function which collects its
    argument into the sequence.  The original `seq' is not
    modified, and is used only as a template.
    See also [[w/collect]] "
  `(symeval!<base>collect-on ,seq (fn (collect) ,@body)))

(def nthcdr (n xs)
  " Returns the result of applying `cdr' repeatedly
    by `n' times on `xs'.
    See also [[cut]] [[firstn]] "
  (if n (nthcdr-internal n (scanner xs))))
(def nthcdr-internal (n xs)
  (if (> n 0) (nthcdr-internal (- n 1) (cdr xs))
              xs))

; scanners
(def scanner (a)
  " Converts `a' to a scanner.
    Sequence-like types should overload this
    function to return a value with a type
    that can be validly used with 'car and 'cdr
    See also [[unscan]] [[car]] [[cdr]] "
  (err "Can't be converted to scanner sequence " a))
(def unscan (origob scan)
  " Converts the scanner `scan' to an object
    of the same type as `origob'.
    Sequence-like types should overload this
    function on `origob'.  The overloaded
    method should ignore the value of `origob'
    and treat `scan' as a read-only scanner.
    Scanner types can validly return `scan'.
    See also [[scanner]] "
  (err "Can't reconstitute " (type origob) " from scanner"))

(defm scanner ((t a cons))
  a)
(defm unscan ((t a cons) scan)
  (w/collect:each i scan
    (collect i)))

(defm scanner ((t a string))
  (coerce a 'cons))
(defm unscan ((t a string) scan)
  ((fn (port)
     (each ch scan
       (disp ch port))
     (inside port))
   (outstring)))

(defm scanner ((t a table))
  (w/collect:maptable
    (fn (k v)
      (collect (cons k v)))
    a))
(defm unscan ((t a table) scan)
  ((fn (rv)
     (each (k . v) scan
       (sref rv v k))
     rv)
   (table)))

(defm scanner ((t a bool))
  (if a (err "Attempt to scan t"))
  nil)
(defm unscan ((t a bool) scan)
  scan)

(let self nil
  (set self
       (fn (f s)
         (if s
             (if (f:car s)
                 (self f (cdr s))))))
  (def <base>each (seq skip bf)
    " Traverses a traversable object `seq' (any object
      which validly returns when applied to 'scanner)
      starting at `skip'.
      The function `bf' is called with a single
      parameter, the value at each position in the
      sequence.
      Traversal continues if the function returns a
      true value, and stops if the function returns
      'nil.
      See also [[<arc>each]] "
    (self bf (nthcdr skip seq))))

(let self nil
  (set self
       (fn (f s i l)
         (if (< i l)
             (if (f s.i)
                 (self f s (+ i 1) l)))))
  (defm <base>each ((t seq string) skip bf)
    (self bf seq skip (len seq))))

(defm <base>each ((t seq table) skip bf)
  (ccc
    (fn (break)
      (maptable
        (fn (k v)
          (if
            (> skip 0)
              (set skip (- skip 1))
            (no (bf:cons k v))
              (break nil)))
        seq))))

(def <base>collect-on (seq bf)
  (unscan seq
    (w/collect-f bf)))
(defm <base>collect-on ((t seq cons) bf)
  (w/collect-f bf))
(defm <base>collect-on ((t seq string) bf)
  (let port (outstring)
    (bf (fn (e) (disp e port) e))
    (inside port)))
(defm <base>collect-on ((t seq table) bf)
  (let rv (table)
    (bf (fn (val)
          (let (k . v) val
            (sref rv v k)
            val)))
    rv))
(defm <base>collect-on ((t seq bool) bf)
  (if seq (err "attempt to collect on t"))
  (w/collect-f bf))

(mac w/scanner-collect-each (var seq . body)
  (let gseq (uniq)
     `((fn (,gseq)
         (symeval!<base>collect-on ,gseq
           (fn (collect)
             (symeval!<base>each ,gseq 0
               (fn (,var) ,@body t)))))
       ,seq)))

; override the default behaviour of 'len
(let self nil
  (set self
       (fn (a i)
         (if a (self (cdr a) (<base>+ i 1))
               i)))
  (defm len (a)
    (self (scanner a) 0)))

; standard stuff

(set >>
  (l-reductor
    (fn () (err "'>> requires at least one parameter"))
    idfn
    (fn (a b) (b a))))
(docstring '>> 'fn' '(val . funs)
  " Chains the value `val' through the functions in `fun'.
    Example:
       (>> 5 g f)
       ==
       (f (g 5))
    See also [[compose]] ")

(def map1 (f xs)
  " Return a sequence with function f applied to every element in sequence xs.
    See also [[map]] [[each]] [[mappend]] [[andmap]] [[ormap]] "
  (w/scanner-collect-each e xs
    (collect:f e)))

(def pair (xs (o f list))
  " Applies pairs of elements to the function `f'.
    See also [[tuples]] [[map]] "
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))

(mac $ (x)
   " Allows access to the underlying Scheme. "
   `(seval ',(unpkg x)))

(def assoc (key al)
  " Finds a (key value) pair in an associated list.
    See also [[alref]] [[listtab]] [[tablist]] "
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key)
  " Get a value from a key in a associated list.
    See also [[assoc]] [[listtab]] [[tablist]] "
  (cadr (assoc key al)))

(mac with (parms . body)
  " Assigns a set of local variables for the given `body'.
    Assignment is simultaneous.
    See also [[withs]] [[given]] [[let]] [[fn]] [[do]] "
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac withs (parms . body)
  " Assigns local variables for the given `body'.
    The assignments are made in the given order.
    See also [[with]] [[givens]] [[let]] [[fn]] [[do]] "
  (if (no parms) 
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms) 
         (withs ,(cddr parms) ,@body))))

(def butlast (seq)
  " Returns every element of `seq' but the last one.
    See also [[last]] [[cut]] "
  (cut seq 0 -1))

(mac given body
  " Simultaneously assigns the given (unparenthesized) local variables in the
    one-statement body.
    See also [[let]] [[with]] [[givens]]"
  (with (args (butlast body)
         expr (last body))
    `(with ,args
       ,expr)))

(mac givens body
  " Sequentially assigns the given (unparenthesized) local variables in the
    one-statement body.
    See also [[let]] [[withs]] [[given]]"
  (with (args (butlast body)
         expr (last body))
    `(withs ,args
       ,expr)))

; Rtm prefers to overload + to do this
; almkglor prefers to do this in 'join

(def join args
  " Joins all scanner arguments together.
    The type of the result is the type of the
    first non-nil argument.
    See also [[cons]] [[+]] "
  (join-inner args))
(def join-inner (args)
  (if (no args)
      nil
      (let a (car args) 
        (if (no a)
          (join-inner (cdr args))
          (w/collect-on a
            (each i a (collect i))
            (each s (cdr args)
              (each i s (collect i))))))))

(mac rfn (name parms . body)
  " Creates a function which calls itself as `name'.
    See also [[fn]] [[afn]] "
  `(let ,name nil
     (set ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  " Creates a function which calls itself with the name `self'.
    See also [[fn]] [[rfn]] [[aif]] [[awhen]] [[aand]] "
  `(rfn self ,parms ,@body))

(def rev (xs) 
  " Reverses a copy of the sequence `xs'
    See also [[copy]] "
  (let v nil
    (each x xs
      (set v (cons x v)))
    (unscan xs v)))

(mac w/uniq (names . body)
  " Assigns a set of variables to unique symbols.
    Generally used for macro functions.
    See also [[uniq]] "
  (if (acons names)
      `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                                 names))
         ,@body)
      `(let ,names (uniq) ,@body)))

(mac or args
  " Computes arguments until one of them is true and returns that result.
    See also [[and]] [[orf]] [[ormap]] [[check]] "
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g (or ,@(cdr args)))))))

(def alist (x)
  " Return true if argument is a possibly empty list
    Unlike 'acons, this function returns t when given an empty list
    See also [[atom]] [[acons]] [[dotted]] [[isa]] [[cons]] [[list]] "
  (or (no x) (is (type x) 'cons)))

(mac in (x . choices)
  " Returns true if the first argument is one of the other arguments.
    See also [[some]] [[mem]] "
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))

(set iso
  (t-reductor
    (fn ()    (err "'iso requires at least one argument"))
    (fn (a)   (fn (b) (iso a b)))
    (fn (a b) (<base>iso a b))))
(docstring 'iso 'fn 'rest
  " Isomorphic compare - compares structure (can be slow).
    See also [[is]] ")
(def <base>iso (x y)
  (<base>is x y))
(IDEAL
 (defm <base>iso ((t x cons) (t y cons))
   (or (<base>is x y)
       (and (iso (car x) (car y))
            (iso (car x) (car y)))))
 REAL
 ; true multimethods not implemented yet T.T
 (defm <base>iso ((t x cons) y)
   (or (<base>is x y)
       (and (acons y) 
            (iso (car x) (car y)) 
            (iso (cdr x) (cdr y))))))

(mac when (test . body)
  " When `test' is true, do `body'.
    See also [[unless]] [[if]] [[awhen]] "
  `(if ,test (do ,@body)))

(mac unless (test . body)
  " When `test' is not true, do `body'.
    See also [[when]] [[if]] [[no]] "
  `(if (no ,test) (do ,@body)))

(mac breakable body
  " Allows a (break ...) form to return a value from within the
    body of a control structure.
    Example:
    (breakable:while t
      (aif (something) (break it)))
    See also [[catch]] [[point]] [[accum]] [[while]] "
  `(symeval!ccc (fn (break) ,@body)))

(mac while (test . body)
  " While `test' is true, perform `body' in a loop.
    See also [[until]] [[loop]] [[whilet]] [[whiler]] [[for]]
    [[repeat]] [[drain]] [[always]] "
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))

(mac always body
  " Executes the contents of `body' in an infinite loop.
    See also [[while]] "
  (w/uniq gf
    `((rfn ,gf ()
        ,@body
        (,gf)))))

(def empty (seq) 
  " Test to see if `seq' is an empty list or other sequence.
    See also [[no]] [[acons]] [[len]] "
  (is (len seq) 0))
(defm empty ((t seq bool))
  (no seq))
(defm empty ((t seq cons))
  nil)

(def reclist (f xs)
  " Applies the function `f' on succeeding `cdr' of
    the sequence `xs' until `f' returns true.
    See also [[ormap]] [[andmap]] [[map]] "
  (reclist-internal f (scanner xs)))
(def reclist-internal (f xs)
  (and xs (or (f xs) (reclist-internal f (cdr xs)))))

(def recstring (test s (o start 0))
  " Applies the function `test' on indices of `s'
    until `test' returns true.
    See also [[map]] [[reclist]] "
  (let n (len s)
    ((afn (i)
       (and (< i (len s))
            (or (test i)
                (self (+ i 1)))))
     start)))

(def isa (x y)
  " Checks if x is of type y.
    See also [[acons]] [[alist]] [[atom]] "
  (is (type x) y))

(def testify (x)
  " Creates a test that determines if a given argument is `x'.
    See also [[is]] "
  (if (isa x 'fn) x (fn (_) (is _ x))))

(def some (test seq)
  (with (rv nil
         f (testify test))
    (each-early-out i seq
      (set rv (f i))
      (no rv))
    rv))

(def all (test seq) 
  " Determines if all elements of `seq' satisfy `test'.
    See also [[andmap]] [[some]] "
  (no (some (complement (testify test)) seq)))
       
(def dotted (x)
  " Determines if `x' is a dotted cons pair.
    See also [[acons]] [[alist]] "
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

; I couldn't find a pre-existing total macro-expander
(def expand (expr)
  " Completely expands all macros in `expr'.
    See also [[macex]] [[mac]] "
  (if (and (acons expr) (~dotted expr) (~is 'quote (car expr)))
      (let expansion (macex (cons (expand (car expr))
                                  (map1 expand (cdr expr))))
        (if (and (acons expansion) (acons:car expansion))
          (cons (expand:car expansion) (cdr expansion))
          expansion))
      expr))

(def makeproper (lst)
  " Transforms `list' to a proper list if it is a dotted list.
    Note that we mean actual cons cells; it does not work with
    scanners.
    See also [[dotted]] [[list]] "
  (if (no (acons lst))
      lst
      (w/collect (makeproper-internal lst collect))))
(def makeproper-internal (lst collect)
  (collect lst))
(defm makeproper-internal ((t lst cons) collect)
  (collect:car lst)
  (makeproper-internal (cdr lst) collect))

(def andmap (pred seq)
  " Applies `pred' to elements of `seq' until an element fails.
    See also [[all]] [[and]] [[andf]] [[map]] "
  (no (some ~pred seq)))

(def ormap (pred seq)
  " Applies `pred' to elements of `seq' until an element passes.
    See also [[some]] [[or]] [[orf]] [[map]] "
  (some pred seq))

; The call* global function defines how to deal with non-functions
; in functional positions.

(mac defcall (name parms . body)
  " Defines a function to run when an object of the given type
    is encountered in functional position.
    The first argument to this function is the `rep' of the object,
    and the rest are passed as arguments to the object.
    See also [[rep]] [[annotate]] [[type]] "
  (w/uniq (gsym grest)
    (if (acons parms)
        `(defm call* ,(cons `(t ,gsym ,name) (cdr parms))
           ((fn (,(car parms))
              ,@body)
            (symeval!rep ,gsym)))
        `(defm call* ,(cons `(t ,gsym ,name) grest)
           (apply (fn ,parms
                      ,@body)
                  (symeval!rep ,gsym)
                  ,grest)))))

(defcall num (num . args)
  (if (acons args)
      (if (or (isa (car args) 'num) (isa (car args) 'int))
        (err:tostring:prn "Number applied to number - " num " - parameters - " args)
        (apply (car args) num (cdr args)))
      num))
(defcall int (num . args)
  (if (acons args)
      (if (or (isa (car args) 'num) (isa (car args) 'int))
        (err:tostring:prn "Number applied to number - " num " - parameters - " args)
        (apply (car args) num (cdr args)))
      num))

; almkglor: simplify 'make-br-fn?
; (mac make-br-fn (expr)
;   (w/uniq rest
;     `(fn ,(cons '(o _) rest)
;        ,expr)))
; - i.e. make it a variadic function
; whose first optional argument is _

(def *mbf-arglist-vars (arglist)
  " Returns the variables bound in an argument list.
    See also [[make-br-fn]] "
  (if (isa arglist 'cons)
    (apply join
      (map1
        (fn (_)
          (if (isa _ 'cons)
            (if (is (car _) 'o)
              (list:cadr _)
              _)
            (list _)))
        (makeproper arglist)))
    arglist))

(def *mbf-arglist-frees (arglist)
  " Returns the free variables used in default values for optional arguments
    of an argument list.
    See also [[make-br-fn]] "
  (if (isa arglist 'cons)
    (apply join
      (map1
        (fn (_)
          (and (isa _ 'cons)
               (is (car _) 'o)
               (*mbf-all-vars (cddr _))))
        (makeproper arglist)))
    nil))

(def *mbf-all-vars (form)
  " Extracts all the variables in the fully macro-expanded s-expression `form'.
    See also [[make-br-fn]] "
  (let head (and (isa form 'cons) (car form))
    (if
      (or (no form) (and (no (isa form 'sym)) (no (isa form 'cons))))
        nil
      (isa form 'sym)
        (list form)
      (is head 'quote)
        nil
      (is head 'quasiquote)
        (ormap
          (fn (_)
            (and (isa _ 'cons)
                 (in (car _) 'unquote 'unquote-splicing)
                 (apply join (map1 *mbf-all-vars (cdr _)))))
          (cdr form))
      (is head 'if)
        (apply join (map1 *mbf-all-vars (cdr form)))
      (is head 'fn)
        (join (apply join (map1 *mbf-all-vars (*mbf-arglist-vars  (cadr form))))
              (apply join (map1 *mbf-all-vars (*mbf-arglist-frees (cadr form))))
              (apply join (map1 *mbf-all-vars                     (cddr form))))
      ; else (including set)
        (apply join (map1 *mbf-all-vars form)))))

(def *mbf-free? (form var)
  " Checks if the variable named `var' occurs free (unbound) in `form'.
    See also [[make-br-fn]] "
  ; I'd like to use case, but it doesn't exist yet.
  (with (kind (type form)
         find (afn (x lst)
                (if (and (alist lst) lst)
                  (or (is x (car lst) (self x (cdr lst))))
                  nil)))
    (if
      (is kind 'sym)
        (is form var)
      (is kind 'cons)
        (let head (car form)
          (if
            (is head 'fn)
              (or (find var (*mbf-arglist-frees (cadr form)))
                  (and (no (find var (*mbf-arglist-vars (cadr form))))
                       (*mbf-free? (cddr form) var)))
            (is head 'quote)
              #f
            (is head 'quasiquote)
              (ormap
                (fn (_)
                  (and (isa _ 'cons)
                       (in (car _) 'unquote 'unquote-splicing)
                       (*mbf-free? (cdr _) var)))
                form)
            ; else
              (ormap (fn (_) (*mbf-free? _ var)) form)))
    ; else
      nil)))

(mac make-br-fn (body)
  " Constructs an anonymous procedure with the body `body'; the procedure will
    have bound variables of the form _1 ... _N, where N is the largest of those
    variables used in the function.  __ will be a rest parameter, and _ is an
    alias for _1.  Each variable is declared iff it is used.  This is used to
    implement the [] anonymous functions. "
  (with (max 0 arbno nil)
    (map1 ; Just for the side-effect; used as "max"
      (fn (_)
        (withs (s (coerce _ 'string) first (s 0) rest (coerce (cdr (coerce s 'cons)) 'string))
          (and (is (s 0) #\_)
               (or (is rest "")
                   (is rest "_")
                   (and (some (fn (c) (isnt c #\0)) rest)
                        (all  (fn (c) (in c #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) rest)))
               (*mbf-free? body _)
               (let num (or (and (is rest "") 1) (and (is rest "_") (do (set arbno t) -1)) (coerce rest 'int))
                 (when (> num max) (set max num)))
               nil)))
      (*mbf-all-vars (expand body)))
    `(fn ,((afn (n)
             (if (< n (+ max 1))
               (cons (coerce (+ "_" (coerce n 'string)) 'sym) (self (+ n 1)))
               (if arbno
                 '__
                 (if (is max 0) '(_) nil)))) 1)
         ,(if (> max 0)
            `(let _ _1 ,body)
            body))))

;;; NO []s ABOVE THIS LINE ;;;

(def mem (test seq)
  " Returns the sublist of `seq' whose first element
    satisfies `test'.
    This function always returns a scanner.
    See also [[find]] [[some]] [[in]]"
  (let f (testify test)
    (reclist [if (f:car _) _] (scanner seq))))

(def find (test seq)
  " Returns the first element that matches the test function.
    See also [[mem]] [[some]] [[in]] "
  (with (rv nil
         f  (testify test))
    (each-early-out i seq
      (if (f i)
          (do (set rv i) nil)
          t))
    rv))

(def map (f . seqs)
  " Applies the elements of the sequences to the given function.
    Returns a sequence containing the results of the function.
    See also [[each]] [[mapeach]] [[map1]] [[mappend]] [[andmap]]
    [[ormap]] [[reduce]] "
  (if (no (cdr seqs)) 
        (map1 f (car seqs))
        (w/collect-on (car seqs)
          ((afn (seqs)
            (if (some no seqs)  
                nil
                (do (collect (apply f (map1 car seqs)))
                    (self (map1 cdr seqs)))))
           (map1 scanner seqs)))))

(def mappend (f . args)
  " Applies the elements of the sequences to the given function.
    Returns a sequence containing the concatenation of the results
    of the function.
    See also [[map]] [[join]] "
  (join-inner (apply map f args)))

(def firstn (n xs)
  " Returns the first `n' elements of the given sequence
    `xs'.
    See also [[cut]] [[nthcdr]] "
  (if (no n)
      xs
      (w/collect-on xs
        ((afn (n xs)
           (if (and (> n 0) xs)
               (do (collect (car xs))
                   (self (- n 1) (cdr xs)))))
         n (scanner xs)))))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  " Returns a list of sequences of the elements of
    the sequence `xs', grouped by `n' elements.
    See also [[pair]] "
  (w/collect
    ((afn (xs)
       (when xs
         (collect:firstn n xs)
         (self:nthcdr n xs)))
     (scanner xs))))

(def caris (x val) 
  " Determines if (car x) is a valid
    operation, and that the result of
    that operation is `val'.
    See also [[is]] [[car]] [[carif]] "
  (breakable
    (is (on-err (fn (_) (break nil))
                (fn () (car x)))
        val)))

(def warn (msg . args)
  " Displays a warning message on its arguments.
    See also [[ero]] [[pr]] "
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
  " Performs `body' atomically, blocking other threads.
    See also [[atlet]] [[atwith]] [[atwiths]] "
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  " Performs a `let' atomically, blocking other threads.
    See also [[atomic]] [[atwith]] [[atwiths]] "
  `(atomic (let ,@args)))
  
(mac atwith args
  " Performs a `with' atomically, blocking other threads.
    See also [[atomic]] [[atlet]] [[atwiths]] "
  `(atomic (with ,@args)))

(mac atwiths args
  " Performs a `withs' atomically, blocking other threads.
    See also [[atomic]] [[atlet]] [[atwith]] "
  `(atomic (withs ,@args)))

; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to withs
;  get is an expression returning the current value in the place
;  set is an expression representing a function of one argument
;   that stores a new value in the place

; A bit gross that it works based on the *name* in the car, but maybe
; wrong to worry.  Macros live in expression land.

; seems meaningful to e.g. (push 1 (pop x)) if (car x) is a cons.
; can't in cl though.  could I define a setter for push or pop?

(set setter (table))

(mac defset (name parms . body)
  " Defines a setter for the named form.
    See also [[=]] "
  (w/uniq gexpr
    `(sref setter 
           (fn (,gexpr)
             (let ,parms (cdr ,gexpr)
               ,@body))
           ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional 
; position.  Such bugs will be seen only when the code is executed, when 
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa expr 'sym)
         (if (ssyntax expr)
             (setforms (ssexpand expr))
             (w/uniq (g h)
               (list (list g expr) 
                     g
                     `(fn (,h) (set ,expr ,h)))))
        ; make it also work for uncompressed calls to compose
        (and (acons expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
         (let f (setter (car expr))
           (if f
               (f expr)
               ; assumed to be data structure in fn position
               (do (when (caris (car expr) 'fn)
                     (warn "Inverting what looks like a function call" 
                           expr0 expr))
                   (w/uniq (g h)
                     (let argsyms (map [uniq] (cdr expr))
                        (list (+ (list g (car expr))
                                 (mappend list argsyms (cdr expr)))
                              `(,g ,@argsyms)
                              `(fn (,h) (sref ,g ,h ,@argsyms)))))))))))

(def metafn (x)
  (and (acons x) (in (car x) 'compose 'complement)))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
      ((afn (fs)
         (if (caris (car fs) 'compose)            ; nested compose
              (self (join (cdr (car fs)) (cdr fs)))
             (cdr fs)
              (list (car fs) (self (cdr fs)))
             (cons (car fs) args)))
       (cdr f))
      (err "Can't invert " (cons f args))))

(def expand= (place val)
  (if (and (isa place 'sym) (~ssyntax place))
      `(set ,place ,val)
      (let (vars prev setter) (setforms place)
        (w/uniq g
          `(atwith ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
  " Assigns values to variables.
    See also [[assert]] [[wipe]] [[++]] [[--]] [[rotate]] [[defset]] "
  (expand=list args))

(mac loop (start test update . body)
  " First performs `start'; while `test' is true, performs `body' then
    `update' in a loop.
    See also [[while]] "
  (w/uniq (gfn gparm)
    `(do ,start
         ((rfn ,gfn (,gparm) 
            (if ,gparm
                (do ,@body ,update (,gfn ,test))))
          ,test))))

(mac for (v init max . body)
  " Loops for the variable `v' from `init' to `max'.
    See also [[repeat]] [[forlen]] "
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (set ,v ,gi) (< ,v ,gm) (set ,v (+ ,v 1))
         ,@body))))

(mac repeat (n . body)
  " Repeats the `body' `n' times.
    See also [[for]] [[forlen]] [[n-of]] "
  `(for ,(uniq) 1 ,n ,@body))

(mac mapeach (var expr . body)
  " Performs `body' for each element of the list returned by `expr',
    with each element assigned to `var'; the result of the last expression
    in `body' is stored in a returned list of values.
    See also [[each]] [[map]] "
  `(map1 (fn (,var) ,@body) ,expr))

; (nthcdr x y) = (cut y x).

(def cut (seq start (o end))
  " Returns a subsequence of the given `seq'
    If `end' is negative,
    it's a 0-based index into the end of the string.
    For example,

      > (cut \"abcde\" 1, -1)
      \"bcd\"

    See also [[firstn]] [[nthcdr]] [[split]] "
  (if (< start 0) (= start (+ (len seq) start)))
  (w/collect-on seq
    (if end
        (do (if (< end 0) (= end (+ (len seq) end)))
            (= end (- end start))
            (each-skip-early-out start i seq
                (if (<= end 0)
                    nil
                    (do (collect i)
                        (= end (- end 1))
                        t))))
        (each-skip start i seq
          (collect i)))))

(def at (lst n)
  " Get the `n'th item of lst, *including* negative indicies.
    See also [[cut]] "
  (lst (mod n (len lst))))

(def prefix (pre str)
  " Determines if `pre' is the same as the first part of `str'. "
  (and (<= (len pre) (len str))
       (iso pre (cut str 0 (len pre)))))
      
(mac ontable (k v h . body)
  " Loops through the entries of table or object `h', with key-value pairs
    assigned to `k' and `v', respectively.
    See also [[each]] [[keys]] "
  `(maptable (fn (,k ,v) ,@body) ,h))

(mac whilet (var test . body)
  " While `test' is true, perform `body' in a loop.
    The result of `test' is assigned to `var'.
    See also [[while]] [[whiler]] [[drain]] "
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))

(def last (xs)
  " Returns the last element of `seq'. "
  (last-internal:scanner xs))
(def last-internal (xs)
  (if (cdr xs)
      (last-internal:cdr xs)
      (car xs)))

(def rem (test seq)
  " Returns a sequence with the elements of `seq' that pass `test' removed.
    See also [[keep]] [[pull]] "
  (keep-internal seq (complement (testify test))))

(def keep (test seq) 
  " Returns a list with the elements of `seq' that pass `test'.
    See also [[rem]] [[pull]] "
  (keep-internal seq test))
(def keep-internal (seq f)
  (w/scanner-collect-each i seq
    (if (f i) (collect i))))

(def trues (f seq) 
  " Returns a list with all `nil's removed.
    See also [[rem]] [[keep]] "
  (w/scanner-collect-each i seq
    (let rv (f i)
      (if rv (collect rv)))))

(mac do1 args
  " Performs the body in sequence, then returns the value of the
    first expression.
    See also [[do]] "
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.
; almkglor: no, the above can't actually be done, because of
;   free variables in expressions

(mac caselet (var expr . args)
  " Matches the result of `expr' to arguments until one matches.
    The result of `expr' is assigned to `var'.
    See also [[case]] [[if]] [[iflet]] "
  (let ex (afn (args)
            (if (no (cdr args)) 
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  " Matches the result of `expr' to arguments until one matches.
    See also [[caselet]] [[if]] "
  `(caselet ,(uniq) ,expr ,@args))

(mac push (x place)
  " Pushes the value `x' on the front of the list in `place'.
    See also [[pop]] [[cons]] "
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
  " Swaps the values of the specified places.
    See also [[rotate]] [[=]] "
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  " Rotates the values of the specified places, from right to left.
    See also [[swap]] [[=]] "
  (with (vars (map [uniq] places)
         forms (map setforms places))
    `(atwiths ,(mappend (fn (g (binds val setter))
                          (+ binds (list g val)))
                        vars
                        forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  " Pops a value from the front of the list in `place'.
    See also [[push]] [[car]] "
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g) 
              (,setter (cdr ,g)))))))

(def adjoin (x xs (o test iso))
  " Returns a list with `x' in front of `xs', unless `test' returns true
    for some element of `xs' when matched with `x'.
    See also [[cons]] [[pushnew]] [[consif]] "
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  " Pushes `x' into the front of the list in `place' unless it is
    already in that list.
    See also [[push]] [[adjoin]] [[cons]] [[consif]] "
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  " Removes all elements that pass `test' from the list in `place'.
    See also [[rem]] [[keep]] "
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac ++ (place (o i 1))
  " Increments `place' by the given increment `i' (defaults to 1).
    See also [[--]] [[zap]] [[=]] "
  (if (isa place 'sym)
      `(= ,place (+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (+ ,val ,gi)))))))

(mac -- (place (o i 1))
  " Decrements `place' by the given decrement `i' (defaults to 1).
    See also [[++]] [[zap]] [[=]] "
  (if (isa place 'sym)
      `(= ,place (- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (- ,val ,gi)))))))

; E.g. (inc x) equiv to (zap + x 1)

(mac zap (op place . args)
  " Modifies `place' with the result of `op' on that `place'.
    See also [[++]] [[--]] [[pull]] [[push]] [[pop]] [[=]]
    [[wipe]] [[assert]] "
  (with (gop    (uniq)
         gargs  (map [uniq] args)
         mix    (afn seqs 
                  (if (some no seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

(mac wipe args
  " Sets each of the given places to nil.
    See also [[assert]] [[zap]] [[=]] "
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac assert args
  " Sets each of the given places to t.
    See also [[wipe]] [[zap]] [[=]] "
  `(do ,@(map (fn (a) `(= ,a t)) args)))

; Can't simply mod pr to print strings represented as lists of chars,
; because empty string will get printed as nil.  Would need to rep strings
; as lists of chars annotated with 'string, and modify car and cdr to get
; the rep of these.  That would also require hacking the reader.  

(def pr args
  " Prints the arguments.
    See also [[prn]] [[warn]] [[ero]] "
  (map1 disp args)
  (car args))

(def prn args
  " Prints the arguments followed by a newline.
    See also [[pr]] [[warn]] [[ero]] "
  (do1 (apply pr args)
       (writec #\newline)))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `then' clause.
    See also [[caselet]] [[whenlet]] [[if]] "
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  " Checks if `expr' is true, and if so, assigns it to `var' and
    performs the `body'.
    See also [[caselet]] [[iflet]] [[when]] [[if]] "
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr . body)
  " Similar to `if' but assigns the result of 'expr' to the variable `it'.
    See also [[if]] [[awhen]] [[aand]] [[afn]] "
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))

(mac awhen (expr . body)
  " Similar to `when' but assigns the result of 'expr' to the variable `it'.
    See also [[when]] [[aif]] [[aand]] [[afn]] "
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  " Similar to `and' but assigns the previous expression to the variable `it'.
    See also [[and]] [[aif]] [[awhen]] [[afn]] "
  (if (no args)
      't 
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(mac accum (accfn . body)
  " Collects or accumulates the values given to all calls to `accfn' within
    `body' and returns a list of those values.  Order is not preserved.
    See also [[w/collect]] [[accums]] [[summing]] "
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       ,gacc)))

(mac accums (accfns . body)
  " Collects or accumulates the values given to all calls to functions
    named in `accfns' in body.  Returns a list of lists of those values.
    Order is not preserved.
    See also [[accum]] "
  (let gaccs (map [uniq] accfns)
    `(withs ,(mappend (fn (gacc accfn)
                        (list gacc 'nil accfn `[push _ ,gacc]))
                      gaccs accfns)
       ,@body
       (list ,@(map [list 'rev _] gaccs)))))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof nil))
  " Repeatedly evaluates `expr' until it returns nil, then returns a list
    of the true values.
    See also [[while]] [[whiler]] [[whilet]] "
  (w/uniq (gacc gdone gres)
    `(with (,gacc nil ,gdone nil)
       (while (no ,gdone)
         (let ,gres ,expr
           (if (is ,gres ,eof)
               (set ,gdone t)
               (push ,gres ,gacc))))
       (rev ,gacc))))

; For the common C idiom while (x = snarfdata) != stopval. 
; Rename this if use it often.

(mac whiler (var expr endval . body)
  " Performs `body' while `expr' is not `endval', assigning the result of
    `expr' to `var'.
    See also [[while]] [[whilet]] [[drain]] "
  (w/uniq (gf ge)
    `(let ,ge ,endval
        ((rfn ,gf (,var)
          (when (and ,var (symeval!no (symeval!is ,var ,ge)))
            ,@body
            (,gf ,expr)))
         ,expr))))
  
;(def macex (e)
;  (if (atom e)
;      e
;      (let op (and (atom (car e)) (eval (car e)))
;        (if (isa op 'mac)
;            (apply (rep op) (cdr e))
;            e))))

(def consif (x y)
  " Adds `x' to the front of the list `y' if `x' is true.
    See also [[cons]] [[if]] [[adjoin]] "
  (if x (cons x y) y))

(def string args
  " Creates a string from its arguments
    See also [[sym]] "
  (join-inner (map [coerce _ 'string] args)))

(def flat (x (o stringstoo))
  " Flattens a nested list.
    See also [[list]] "
  ((rfn f (x acc)
     (if (or (no x) (and stringstoo (is x "")))
          acc
         (and (atom x) (no (and stringstoo (isa x 'string))))
          (cons x acc)
         (f (car x) (f (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  " Returns `x' if it passes `test', otherwise returns `alt'.
    See also [[or]] "
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  " Returns the position of the first element in `seq' that passes `test'.
    See also [[some]] "
  (with (f   (testify test)
         rv  nil)
    (each-skip-early-out start i seq
      (if (f i)
          (do (set rv start)
              nil)
          (do (++ start)
              t)))
    rv))

(def even (n) " Determines if a number is even.  See also [[odd]] " (is (mod n 2) 0))

(def odd (n) " Determines if a number is odd.  See also [[even]] " (no (even n)))

(mac after (x . ys)
  " Ensures that the body is performed after the expression `x',
    even if it fails.
    See also [[do]]"
  `(protect (fn () ,x) (fn () ,@ys)))

(let expander 
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  ; TODO: fix these to use 'dynamic-wind instead:
  ; have the exit function save the conditions of
  ; the stream to be closed (i.e. file position)
  ; have the enter function check for saved
  ; conditions and restore them if so.
  ; This will allow them to be safely used in
  ; coroutines.
  (mac w/infile (var name . body)
    " Opens the given file `name' for input, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/outfile]] [[w/instring]] [[w/stdin]] [[w/socket]] "
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    " Opens the given file `name' for output, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/infile]] [[w/appendfile]] [[w/outstring]] [[w/stdout]] "
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    " Opens the given string `str' for input, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/outstring]] [[fromstring]] [[w/infile]] [[w/stdin]]
      [[w/socket]] "
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    " Opens the port for listening, assigning the stream to `var'.
      The stream is automatically closed on exit from the `body'.
      See also [[w/infile]] [[w/instring]] [[w/stdin]] "
    (expander 'open-socket var port body))
  )

(mac w/outstring (var . body)
  " Opens a string for output, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    The contents of the string can be accessed via (inside `var')
    See also [[w/instring]] [[tostring]] [[w/outfile]] [[w/stdout]] "
  `(let ,var (outstring) ,@body))

(mac w/appendfile (var name . body)
  " Opens a file `name' for append, assigning the stream to `var'.
    The stream is automatically closed on exit from the `body'.
    See also [[w/outfile]] [[w/infile]] "
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

; rename this simply "to"?  - prob not; rarely use

(mac w/stdout (str . body)
  " Opens the stream `str' for output; normal printed output from `body'
    is redirected to the stream.
    See also [[w/stdin]] [[w/outfile]] [[w/outstring]] "
  `(call-w/stdout ,str (fn () ,@body)))

(mac w/stdin (str . body)
  " Opens the stream `str' for input; normal read input from `body'
    is redirected from the stream.
    See also [[w/stdout]] [[w/infile]] [[w/instring]] [[w/socket]] "
  `(call-w/stdin ,str (fn () ,@body)))

(mac tostring body
  " Returns the printed standard output from `body' as a string.
    See also [[fromstring]] [[w/stdout]] [[w/outstring]] "
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(mac fromstring (str . body)
  " Redirects read standard input to `body' from the given string `str'.
    See also [[tostring]] [[w/stdin]] [[w/instring]] "
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(def readstring1 (s (o eof nil))
  " Reads a single expression from the string.
    See also [[read]] "
  (w/instring i s (read i eof)))

(def read ((o s (stdin)) (o eof))
  " Reads a single expression from a string or stream.
    See also [[readstring1]] [[readfile]] [[readfile1]] [[readall]] "
  (sread s eof))
(defm read ((t s string) (o eof))
  (readstring1 s eof))

(def readfile (name)
  " Reads the expressions from the file `name', and returns a list of
    expressions read from the file.
    See also [[read]] "
  (w/infile s name (drain (read s))))

(def readfile1 (name)
  " Reads a single expression from the file `name'.
    See also [[read]] "
  (w/infile s name (read s)))

(def writefile1 (val name)
  " Writes the value to the file `name'.
    See also [[writefileraw]] "
  (w/outfile s name (write val s)) val)

(def writefileraw (val name) 
  " Write a list of bytes in val to a file.
    See also [[writefile1]] "
  (w/outfile s name (map [writeb _ s] val)))

(def readall (src (o eof nil))
  " Reads the expressions from the string or stream `src', and returns a
    list of expressions read from the file.
    See also [[read]] "
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def sym (x)
  " Returns the symbol for `x'.
    See also [[string]] "
  (coerce x 'sym))

(mac rand-choice exprs
  " Returns the result of one of the given `exprs', chosen at random.
    See also [[random-elt]] "
  `(case (rand ,(len exprs))
     ,@(let key -1 
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  " Repeats `expr' `n' times, then returns the results in a list.
    See also [[repeat]] "
  (w/uniq collect
    `(w/collect-f
        (fn (,collect)
          (repeat ,n (,collect ,expr))))))

(def rand-string (n)
  " Generates a random string of letters and numbers, starting with a letter. "
  (with (cap (fn () (+ 65 (rand 26)))
         sm  (fn () (+ 97 (rand 26)))
         dig (fn () (+ 48 (rand 10))))
    (coerce (map [coerce _ 'char]
                 (cons (rand-choice (cap) (sm))
                       (n-of (- n 1) (rand-choice (cap) (sm) (dig)))))
            'string)))

(mac forlen (var s . body)
  " Loops across the length of the sequence `s'.
    See also [[repeat]] [[each]] [[on]] "
  `(for ,var 0 (- (len ,s) 1) ,@body))

(mac on (var s . body)
  " Loops across the sequence `s', assigning each element to `var',
    and providing the current index in `index'.
    See also [[each]] [[forlen]] "
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      `(symeval!on-f ,s (fn (,var index) ,@body))))
(def on-f (s bf)
  (let index 0
    (each i s
      (bf i index)
      ++.index)))

(def best (f seq)
  " Selects the best element of `seq' according to `f'.
    `f' is a comparison function between elements of `seq'.
    See also [[max]] [[most]] "
  (if (no seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt)))
        wins)))
              
(def max args
  " Returns the highest argument.
    See also [[min]] [[best]] [[most]] "
  (best > args))
(def min args
  " Returns the lowest argument.
    See also [[max]] [[best]] [[most]] "
  (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     `(with (,a ,x ,b ,y) (if (> ,a ,b) ,a ,b))))

(def most (f seq) 
  " Selects the element of `seq' with the highest [f _].
    `f' is a score function for elements of `seq'.
    See also [[best]] [[least]] "
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

(def least (f seq)
  " Selects the element of `seq' with the lowest [f _].
    `f' is a score function for elements of `seq'.
    See also [[most]] "
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (< score topscore) (= wins elt topscore score))))
      wins)))

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a 
; macroexpansion.
  
; almkglor: these are part of the <arc>vN-sort-internals
(def insert-sorted (test elt seq)
  " Inserts `elt' into a sequence `seq' sorted by `test'.
    See also [[sort]] [[insort]] [[reinsert-sorted]] "
  (if (no seq)
       (list elt) 
      (test elt (car seq)) 
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  " Inserts `elt' into a sequence in the place `seq' sorted by `test'.
    See also [[insert-sorted]] [[sort]] "
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  " Inserts `elt' into a sequence `seq', partially sorted by `test'.
    See also [[insert-sorted]] [[insortnew]] [[sort]] "
  (if (no seq) 
       (list elt) 
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq)) 
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  " Inserts `elt' into a sequence in the place `seq', partially sorted
    by `test'.
    See also [[reinsert-sorted]] [[sort]] "
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

; Could make this look at the sig of f and return a fn that took the 
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
  " Creates a function that will store results of calls to the given
    source function.
    For each set of arguments, the source function will only be called
    once; if the memo'ed function is called again with the same arguments,
    it will return the stored result instead of calling the source function.
    See also [[defmemo]] "
  (let cache (table)
    (fn args
      (or (cache args)
          (= (cache args) (apply f args))))))

(mac defmemo (name parms . body)
  " Defines a function that automatically stores the results of calls.
    For each set of arguments, this function will only execute once.
    If the function is called again with the same arguments, it will
    immediately return the stored result for that set of arguments.
    See also [[memo]] "
  `(safeset ,name (memo (fn ,parms ,@body))))

(def whitec (c)
  " Determines if the given `c' is a whitespace character.
    See also [[alphadig]] [[nonwhite]] [[punc]] "
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c)
  " Determines if the given `c' is not a whitespace character.
    See also [[whitec]] [[alphadig]] [[punc]] "
  (no (whitec c)))

(def alphadig (c)
  " Determines if the given `c' is an alphanumeric character.
    See also [[whitec]] [[nonwhite]] [[punc]] "
  (or (<= #\a c #\z) (<= #\A c #\Z) (<= #\0 c #\9)))

(def punc (c)
  " Determines if the given `c' is punctuation character.
    See also [[whitec]] [[nonwhite]] [[alphadig]] [[punc]] "
  (in c #\. #\, #\; #\: #\! #\?))

(def readline ((o str (stdin)))
  " Reads a string terminated by a newline from the stream `str'. "
  (when (peekc str)
    (tostring 
      ; can/should be improved by making this seamless check for
      ; \r, \r\n, \n, or \n\r terminators
      (whiler c (readc str) #\newline
        (writec c)))))

; Don't currently use this but suspect some code could.

(mac summing (sumfn . body)
  " Counts the number of times `sumfn' is called with a true value
    within `body'.
    See also [[accum]] "
  (w/uniq (gc gt)
    `(let ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body)
       ,gc)))

(def treewise (f base tree)
  " Traverses a list as a binary tree.
    See also [[trav]] [[tree-subst]] [[ontree]] "
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree)) 
         (treewise f base (cdr tree)))))

(def carif (x)
  " Returns the first element of a list if the argument
    is a scanner.
    See also [[car]] [[caris]] "
  (on-err (fn (c) nil)
          (fn () (car x))))

; Could prob be generalized beyond printing.

(def prall (elts (o init "") (o sep ", "))
  " Prints several arguments with an initial header and separated by a
    given separator.
    See also [[prs]] "
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))
             
(def prs args     
  " Prints several arguments separated by spaces.
    See also [[prall]] "
  (prall args "" #\space))

(def tree-subst (old new tree)
  " Replaces an element of a list with that list treated as a binary tree.
    See also [[treewise]] [[trav]] "
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  " Applies a function across each node of a list with that list treated
    as a binary tree.
    See also [[treewise]] [[trav]] "
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def fill-table (table data)
  " Fills `table' with key-value pairs in the `data' list.
    See also [[table]] "
  (each (k v) (pair data) (= (table k) v))
  table)

(mac obj args
  " Creates an object with the specified entries.
    See also [[inst]] [[table]] "
  (w/uniq g
    `(let ,g (table)
       ,@(map (fn ((k v)) `(= (,g ',k) ,v))
              (pair args))
       ,g)))

(def keys (h) 
  " Returns a list of keys in the table or object `h'.
    See also [[vals]] [[table]] "
  (w/collect (ontable k v h (collect k))))

(def vals (h) 
  " Returns a list of values in the table or object `h'.
    See also [[keys]] [[table]] "
  (accum a (ontable k v h (a v))))

; These two should really be done by coerce.  Wrap coerce?

(def tablist (h)
  " Transforms a table or object `h' into an association list.
    See also [[listtab]] [[alref]] [[assoc]] "
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
  " Transforms an association list into a table or object.
    See also [[tablist]] [[alref]] [[assoc]] "
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(def load-table (file (o eof))
  " Loads an association list from `file' into a table or object.
    See also [[load-tables]] [[read-table]] [[save-table]] [[listtab]] "
  (w/infile i file (read-table i eof)))

(def read-table ((o i (stdin)) (o eof))
  " Loads an association list from the stream `i' into a table or object.
    See also [[load-tables]] [[load-table]] [[write-table]] [[listtab]] "
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
  " Loads several association lists from `file' into a list of tables or
    objects.
    See also [[load-table]] [[read-table]] "
  (w/infile i file
    (w/uniq eof
      (drain (read-table i eof) eof))))

(def save-table (h file)
  " Writes a table or object `h' to `file'.
    See also [[write-table]] [[load-table]] [[tablist]] "
  (w/outfile o file (write-table h o)))

(def write-table (h (o o (stdout)))
  " Writes a table or object `h' to the stream `o'.
    See also [[save-table]] [[read-table]] [[tablist]] "
  (write (tablist h) o))

(def copy (x . args)
  " Creates a copy of an existing argument `x'.
    Additional arguments replace specified
    indices.
    Note that this function has assumptions
    that are not exactly in-line with the
    `scanner' idiom in Arc-F
    See also [[rev]] [[copy-seq]] "
  (let x2 (case (type x)
            sym    x
            cons   (apply (fn args args) x)
            string (let new (newstring (len x))
                     (forlen i x
                       (= (new i) (x i)))
                     new)
            table  (let new (table)
                     (ontable k v x 
                       (= (new k) v))
                     new)
                   (err "Can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (pair args))
    x2))

(def copy-seq (x)
  " Creates a copy of the sequence `seq' "
  (w/scanner-collect-each i x (collect i)))

(def seq-to-list (x)
  " Converts the sequence `seq' to a true list "
  (w/collect:each i x
    (collect i)))

(def abs (n)
  " Returns the absolute value of a number.
    See also [[signop]] "
  (if (< n 0) (- n) n))

(def signop (n)
  " Returns the sign of a number as the function `+' or `-'.
    See also [[abs]] "
  (if (< n 0) - +))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

(def round (n)
  " Rounds off a fractional value to the nearest whole number.
    See also [[roundup]] [[to-nearest]] "
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  " Rounds off a fractional value to the nearest absolute highest
    whole number.
    See also [[round]] [[to-nearest]] "
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2) 
        ((if (> n 0) + -) base 1)
        base)))

(def to-nearest (n quantum)
  " Rounds off `n' to the nearest multiple of `quantum'.
    See also [[round]] [[roundup]] "
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) " Averages all numbers in `ns'. " (/ (apply + ns) (len ns)))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1) 

(def sort (test seq)
  " Sorts `seq' according to `test'. "
  (unscan seq
    (mergesort test (seq-to-list seq))))

; Destructive stable merge-sort, adapted from slib and improved 
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  " Destructively sorts a list `lst' according to `less?'. "
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

; Also by Eli.

(def merge (less? x y)
  " Merges two sorted lists by `less?'. "
  (if (no x) y
      (no y) x
      (let lup nil
        ; hot spot
        (set lup
             (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
               (if (less? (car y) (car x))
                 (do (if r-x? (scdr r y))
                     (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                 ; (car x) <= (car y)
                 (do (if (no r-x?) (scdr r x))
                     (if (cdr x) (lup x (cdr x) y t) (scdr x y))))))
        (if (less? (car y) (car x))
          ; hot spot
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          ; hot spot (hotter than true branch above)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def bestn (n f seq)
  " Returns a list of the best `n' elements of seq according to
    the comparison function `f'.
    See also [[best]] "
  (firstn n (sort f seq)))

(def split (seq pos)
  " Destructively splits the cons list `seq' at offset `pos',
    returning a two-element list of the split.
    See also [[cut]] "
  (if (is 0 pos)
      (list nil seq)
      (withs (mid (nthcdr (- pos 1) seq) 
              s2  (cdr mid))
        (wipe (cdr mid))
        (list seq s2))))

(def ssplit (str (o delim whitec) (o keepdelim) (o noblanks))
  "Split `str' on chars passing the test `delim', returning a list of
   strings.  If `keepdelim' is non-nil include the delimiters.  If
   `noblanks' is non-nil empty strings are excluded."
  (if (isa delim 'string) (= delim [in _ (coerce delim 'cons)]))
  (with (acc nil j 0)
    (forlen i str
      (if (and (or (no keepdelim) (> i 0))
                   (delim (str i)))
           (do (push (cut str j i) acc)
               (= j (if keepdelim i (+ i 1)))))
      (if (and (atend i str)
               (<= j i))
          (push (cut str j (+ i 1)) acc))) ; add 1 because atend is true prematurely
    (rev (if noblanks (rem empty acc) acc))))

(mac time (expr)
  " Prints the time consumed by the `expr', returning the result. "
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (prn "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
  " Prints the time consumed by `expr', returning `ok' when the
    expression completes. "
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  " Prints the time consumed by executing `expr' 10 times "
  `(time (repeat 10 ,expr)))

(def union (f xs ys)
  (w/collect-on xs
    (each x xs (collect x))
    (each y ys (if (~some [f _ y] xs)
                   (collect y)))))

(= templates* (table))

(mac deftem (tem . fields)
  " Defines an object template for field values, with inclusion for
    existing templates.
    See also [[inst]] [[templatize]] [[temread]] [[temload]] [[temloadall]] "
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name) 
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name) 
      (union (fn (x y) (is (car x) (car y)))
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (pair fields)))
             (templates* ',name))))

(def inst (tem . args)
  " Creates an object instantiating a given template.
    See also [[deftem]] [[templatize]] [[temread]] [[temload]] [[temloadall]] "
  (let x (table)
    (each (k v) (templates* tem)
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  " Reads an association list from the stream `str' and creates an
    object instantiating the given template containing the data in
    the association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temload]] [[temloadall]] "
  (templatize tem (read str)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  " Creates an object instantiating a given template containing the
    data in the association list `raw'.
    See also [[deftem]] [[inst]] [[temread]] [[temload]] [[temloadall]] "
  (with (x (inst tem) fields (templates* tem))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  " Reads an association list from `file' and creates an object
    instantiating the given template containing the data in the
    association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temloadall]] "
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  " Reads all association lists from `file' and creates a list
    of objects instantiating the given template containing the
    data in each association list.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temload]]"
  (map (fn (pairs) (templatize tem pairs)) 
       (w/infile in file (readall in))))

(def tems ()
  " Pretty print templates defined in `templates*'.
    See also [[deftem]] [[inst]] [[templatize]] [[temread]] [[temload]] "
  (each k (keys templates*)
    (prn k " " (map car (templates* k)))))

(def number (n) " Determines if `n' is a number. " (in (type n) 'int 'num))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))

(def hours-since (t1) (/ (since t1) 3600))

(def days-since (t1) (/ (since t1) 86400))

(def cache (timef valf)
  " Caches the result of a call to `valf' until a number of seconds
    greater than the result of a call to `timef' have passed. "
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac errsafe (expr)
  " Executes `expr' and blocks any errors "
  `(on-err (fn (c) nil)
           (fn () ,expr)))

(def saferead (arg)
  " Reads an expression, blocking any errors. "
  (errsafe (read arg)))

(def safe-load-table (filename) 
  " Loads a table from `filename', blocking any errors. "
  (or (errsafe (load-table filename))
      (table)))

(def mkdir (path (o parents))
   " Creates a directory.
     If `parents' is non-nil, parent directories are created as needed."
  ((let os (which-os)
     (if
       ; If we're running Unix, MzScheme <371 has a bug
       ; where make-directory sets the sticky bit.
       ; Thus, we want to use system instead.
       (or (is os 'unix) (is os 'macosx))
        [system (string "mkdir " (if parents "-p ") _)]

       parents make-directory*
       make-directory))
   path))

(def ensure-dir (path)
  " Ensures that the specified directory exists, and creates it if not
    yet created. "
  (unless (dir-exists path)
          (mkdir path t)))

(def pad (val digits (o char #\ ))
  (= val (string val))
  (string (n-of (- digits (len val)) char) val))

(def date ((o time (seconds)))
  " Returns the date as a string in YYYY-MM-DD format. "
  (let date (datetbl time)
    (string (pad (date 'year) 4 #\0) "-"
            (pad (date 'month) 2 #\0) "-"
            (pad (date 'day) 2 #\0))))

(def count (test x)
  " Counts the number of elements in `x' which pass `test'. "
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  " Trims a string `str' with `...' if it is longer than the given `limit'. "
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def random-elt (seq) 
  " Returns an element of `seq' chosen by random.
    See also [[rand-choice]] "
  (withs (l (len seq)
          r (rand l))
    (each-skip-early-out r i seq
      (= r i)
      nil)
    r))

(mac until (test . body)
  " While `test' is false, perform `body' in a loop.
    See also [[while]] "
  `(while (symeval!no ,test) ,@body))

(def before (x y seq (o i 0))
  " Determines if `x' exists before `y' in `seq'. "
  (with (x  (testify x)
         y  (testify y)
         rv nil)
    (each-skip-early-out i v seq
      (if
        (y i)
          nil
        (x i)
          (do (assert rv)
              nil)
        ; else just iterate
          t))
    rv))

(def par (f . args)
  " Partially apply `f' to `args'; i.e., return a function which, when called,
    calls `f' with `args' and the arguments to the new function. "
  (fn newargs (apply f (join args newargs))))

;(def orf fns
;  " Creates a function which returns true on its argument if any of the
;    given `fns' return true on that argument. "
;  (fn (x) (some [_ x] fns)))

;(def andf fns
;  " Creates a function which returns true on its argument if all of the
;    given `fns' return true on that argument. "
;  (fn (x) (all [_ x] fns)))

(def atend (i s)
  " Determines if the index `i' is at or beyond the end of the sequence `s'. "
  (> i (- (len s) 2)))

(def multiple (x y)
  " Determines if `x' is a multiple of `y'. "
  (is 0 (mod x y)))

(mac nor args
  " Computes arguments until one of them returns true, then returns nil,
    or else returns true. "
  `(symeval!no (or ,@args)))

; Consider making the default sort fn take compare's two args (when do 
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
  " Creates a function that compares using `comparer' the result of `scorer'
    on its arguments. "
  (fn (x y) (comparer (scorer x) (scorer y))))

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

; (def only (f g . args) (aif (apply g args) (f it)))

(def only (f) 
  (fn args (if (car args) (apply f args))))

(mac conswhen (f x y)
  " Adds `x' to the front of `y' if `x' passes the test `f'. "
  (w/uniq (gf gx)
   `(with (,gf ,f ,gx ,x)
      (if (,gf ,gx) (symeval!cons ,gx ,y) ,y))))

; Could rename this get, but don't unless it's frequently used.
; Could combine with firstn if put f arg last, default to (fn (x) t).

(def firstn-that (n f xs)
  " Returns the first `n' elements of `xs' which pass `f'. "
  (w/collect-on xs
    (each-early-out i xs
      (if
        (<= n 0)
          nil
        (f i)
          (do (collect i)
              --.n)
          t))))

(def dedup (xs)
  " Removes duplicated elements from `xs'. "
  (let h (table)
    (w/collect:each x xs
      (unless (h x)
        (collect x)
        (assert (h x))))))

(def single (x)
  " Determines if `x' is a sequence with only one element. "
  (breakable:no (cdr (on-err (fn (c) (break nil))
                             (fn ()  (scanner x))))))

(def intersperse (x ys)
  " Inserts `x' between elements of `ys'. "
  (let first t
    (w/scanner-collect-each y ys
      (if first
        (wipe first)
        (collect x))
      (collect y))))

(def counts (seq (o c (table)))
  " Returns a table with elements of `seq' as keys and the number of
    occurences of that element as values. "
  (each i seq
    (zap [if _ (+ _ 1) 1] (c i)))
  c)

(def commonest (seq)
  " Returns a two-element list containing the most common element of
    `seq' and the number of times it occured in the sequence. "
  (with (winner nil n 0)
    (ontable k v (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

; I cleaned up PG's really, really weird version of
; 'reduce, which has really weird behavior in the case
; when xs is empty and init is not given
; @PG: Stop being a Larry Wall.  Not everyone will
; need your particular peculiar specializations.
; <insert snarky comment here about PG's lousy coding>

(w/uniq initsym
  ; Left-associative
  (def reduce (f xs (o init initsym))
    " Applies `f' to an accumulated result on the elements of `xs'.
      Elements are processsed left-to-right. "
    (let (init . xs)
         (if (is init initsym)
             xs
             (cons init xs))
      (each i xs
        (zap f init i))
      init))

  ; Right-associative
  ; Rather inefficent due to recursive call not being in the tail position.
  ; also because of scanner conversion
  (def rreduce (f xs (o init initsym))
    " Applies `f' to an accumulated result on the elements of `xs'.
      Elements are processed right-to-left. "
    (if (no xs)
        (if (isnt init initsym) init)
        ((afn (xs)
           (if
             (cdr xs)
               (f (car xs) (self:cdr xs))
             (is init initsym)
               (car xs)
               (f (car xs) init)))
         (scanner xs)))))

(let argsym (uniq)

  (def parse-format (str)
    " Parses a simple ~-format string. "
    (rev (accum a
           (with (chars nil  i -1)
             (w/instring s str
               (whilet c (readc s)
                 (case c 
                   #\# (do (a (coerce (rev chars) 'string))
                           (wipe chars)
                           (a (read s)))
                   #\~ (do (a (coerce (rev chars) 'string))
                           (wipe chars)
                           (readc s)
                           (a (list argsym (++ i))))
                       (push c chars))))
              (when chars
                (a (coerce (rev chars) 'string)))))))
  
  (mac prf (str . args)
    " Prints according to a format string, replacing ~* with arguments. "
    `(let ,argsym (list ,@args)
       (pr ,@(parse-format str))))
)

(wipe load-file-stack*)
(def load (file (o hook idfn))
  " Reads the expressions in `file' and evaluates them.  Read expressions
    may be preprocessed by `hook'.
    See also [[require]]. "
  (atwith (file (load-resolve file)
           context (cxt))
    (push current-load-file* load-file-stack*)
    (= current-load-file* file)
    (after
      (w/infile f file
        (if (is (downcase (cut file -5)) ".larc")
          (w/instring p (load-literate-arc f)
            (load-slurp p context hook))
          (load-slurp f context hook)))
      (do (= current-load-file* (pop load-file-stack*))
          nil))))

(def load-slurp (p context hook)
  (with (e   nil
         eof (uniq))
    (while (isnt (= e (read p eof)) eof)
      (eval (hook:cxt-ref-d context e)))))

(def load-literate-arc (p)
  (let (docs maybe-code code
        bldg bldg-section) nil
    (= docs
       (fn ()
         (let l (readline p)
           (if
             (no l)
               ()
             (empty-line l)
               (maybe-code)
               (docs))))
       maybe-code
       (fn ()
         (let l (readline p)
           (if
             (no l)
               ()
             (empty-line l)
               (maybe-code)
             (indented-line l)
               (do (= bldg-section (outstring))
                   (disp l bldg-section)
                   (disp #\newline bldg-section)
                   (code))
               (docs))))
       code
       (fn ()
         (let l (readline p)
           (if
             (no l)
               (do (disp (inside bldg-section) bldg)
                   ())
             (empty-line l)
               (do (disp (inside bldg-section) bldg)
                   (maybe-code))
             (indented-line l)
               (do (disp l bldg-section)
                   (disp #\newline bldg-section)
                   (code))
               (docs)))))
    (= bldg (outstring))
    (docs)
    (inside bldg)))

(def empty-line (l)
  (all whitec l))
(def indented-line (l)
  (or (space-indented-line l)
      (tab-indented-line l)))
(def space-indented-line (l)
  (and (>= (len l) 4)
       (is #\space l.0 l.1 l.2 l.3)))
(def tab-indented-line (l)
  (and (>= (len l) 1)
       (is #\tab l.0)))

(= required-files* (table))
(= (required-files* (load-resolve "arc.arc")) t)

(def require what
  " Loads `file' if it has not yet been `require'd.  Can be fooled by changing
    the name ((require \"foo.arc\") as opposed to (require \"./foo.arc\")), but
    this should not be a problem.
    See also [[load]]. "
  (err "'require can't understand arguments:" what))

(defm require ((t file string))
  (let file (load-resolve file)
    (or (required-files* file)
        (do
          (= (required-files* file) t)
          (load file)))))

(def positive (x)
  " Determines if `x' is a number and is positive. "
  (and (number x) (> x 0)))

(mac w/table (var . body)
  " Creates a table assigned to `var' for use in `body'. "
  `(let ,var (table) ,@body ,var))

(def ero args
  " Outputs `args' to error output. "
  (w/stdout (stderr) 
    (write (car args))
    (each a (cdr args)
      (writec #\space)
      (write a))
    (writec #\newline))
  (car args))

(def lock ()
  " Creates a mutex lock.
    See also [[thread]] [[w/lock]] "
  (let rv (sema)
    (sema-post rv)
    (annotate 'lock
      rv)))

(mac w/lock (l . body)
  " Attempts to acquire a single mutex lock.
    A single thread should not attempt to
    acquire the same lock more than once, or
    it will deadlock itself.
    Care must be taken when acquiring
    multiple locks.
    See also [[lock]] "
  `(symeval!w/lock-f ,l (fn () ,@body)))

(def w/lock-f (l bf)
  (err "'w/lock attempted to lock a non-lock object"))
(defm w/lock-f ((t l lock) bf)
  (let sm (rep l)
    (dynamic-wind
      (fn () (sema-wait sm))
      bf
      (fn () (sema-post sm)))))

(def queue ()
  " Creates a queue.
    See also [[enq]] [[deq]] [[qlen]] [[qlist]] [[enq-limit]]"
  (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

(def enq (obj q)
  " Adds `obj' to a queue.  See also [[queue]] "
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  " Removes and returns an item from a queue.  See also [[queue]] "
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) " Returns the number of items in a queue.  See also [[queue]] "
  (q 2))

(def qlist (q) " Returns the queue contents as a list.  See also [[queue]] "
  (car q))

;; unsafe - suppose we have (enq-limit x q 10) and (enq-limit x q 1000)
;; somewhere else?
(def enq-limit (val q (o limit 1000))
  " Adds an item to the queue; removes a queue item if `limit' is
    exceeded.  See also [[queue]] "
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  " Computes the median of an unsorted list. "
  ((sort > ns) (trunc (/ (len ns) 2))))

(mac noisy-each (n var val . body)
  " Performs `body' for each element of the sequence returned by `expr',
    with each element assigned to `var'; prints a `.' every `n' elements. "
  `(symeval!noisy-each-f ,val ,n (fn (,var) ,@body)))
(def noisy-each-f (seq n bf)
  (let i 0
    (each e seq
      (when (multiple (++ i) n)
        (pr #\.))
      (bf e))
    (prn)))

(mac point (name . body)
  " Creates a form which may be exited by calling `name' from within `body'.
    See also [[catch]] [[breakable]] "
  `(symeval!ccc (fn (,name) ,@body)))

(mac catch body
  " Catches any value returned by `throw' within `body'.
    See also [[breakable]] [[point]] "
  `(point throw ,@body))

(def downcase (x)
  " Converts `x' to lowercase, if a character, string, or symbol;
    otherwise, raises an error. "
  (err "'downcase expects a character, string, or symbol"))
     ; TODO: unicode support
(let downc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                   (coerce (+ n 32) 'char)
                   c)))
  (defm downcase ((t x char))
    (downc x))
  (defm downcase ((t x string))
    (map downc x))
  (defm downcase ((t x sym))
    (sym (map downc (string x)))))

(def upcase (x)
  " Converts `x' to uppercase, if a character, string, or symbol;
    otherwise, raises an error. "
  (err "'upcase expects a character, string, or symbol"))
     ; TODO: unicode support
(let upc (fn (c)
           (let n (coerce c 'int)
             (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                 (coerce (- n 32) 'char)
                 c)))
  (defm upcase ((t x char))
    (upc x))
  (defm upcase ((t x string))
    (map upc x))
  (defm upcase ((t x sym))
    (sym (map upc (string x)))))

(def range (start end (o step 1))
  "Return a range of numbers from `start' to `end', by `step'."
  (given stopcond (if (> step 0) > <)
         acc      (cons start nil)
    (unless (stopcond start end)
      ((afn (acc tl n)
         (if (stopcond n end)
             acc
             (self acc (= (cdr tl) (cons n nil)) (+ n step))))
       acc acc (+ start step)))))

(def mismatch (s1 s2)
  " Returns the first index where `s1' and `s2' do not match. "
  (breakable:let s2 (scanner s2)
    (on i s1
      (unless (is i (car s2))
        (break index))
      (zap cdr s2))))

(def memtable (ks)
  " Creates a membership table which returns t for each element in `ks' and
    nil otherwise. "
  (let h (table)
    (each k ks (assert (h k)))
    h))

(= bar* " | ")

(mac w/bars body
  " Prints out the strings printed by each expression in `body',
    separated by vertical bars. "
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (assert ,needbars)
                                 (pr ,out))))))
                  body)))))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(mac thread body 
  " Launches the expressions in `body' in a new thread, returning the
    thread ID for that thread. "
  `(new-thread (fn () ,@body)))

(mac trav (x . fs)
  " Traverses an object `x'; the object is applied to each function
    in `fs', and sub-nodes of the object may be traversed by
    (self <node>) in any of the functions.
    See also [[trav+]] [[treewise]] [[ontree]] "
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(mac trav+ ((go n) s . body)
  " Traverses an object `s'; the object is named by `n' and sub-nodes
    of the object may be traversed by (`go' ...) in `body'.
    See also [[trav]]
    p.s. a more lisplike version of pg's trav "
  `((rfn ,go (,n)
      (when ,n ,@body))
     ,s))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  `(symeval!sref symeval!hooks* (fn ,@rest) ',name))
  
(mac varif (name (o default))
  "Returns the value of the variable `name' if it exists, or `default'
   otherwise."
  `(if (symeval!bound ',name) ,name ,default))

(mac redef (name parms . body)
  " Redefine a function.  The old function definition may be used within
    `body' as the name `old'. "
  `(do (tostring
        (let old (varif ,name nilfn)
          (= ,name (fn ,parms ,@body))))
       ,name))

(redef table args
  " Creates a table initializing table entries from passed
    key-value pairs.
    See also [[obj]] [[inst]] "
  (let tb (old)
    (fill-table tb args)
    tb))

(mac help ( (o name 'help))
   " Prints the documentation for the given symbol.  To use, type
     (help symbol) ; you may also use (help \"string\") to search
     all documentation for that string.
     See also [[docstring]] "
   (if
     (isa name 'sym)
       `(do (pr ,(helpstr name))
            nil)
     (isa name 'string)
       `(helpsearch (downcase ,name))
     t
       `(do (pr ,(helpstr 'help)) nil) ))

(def helpsearch (str)
  " Prints all symbols whose documentation matches or partly matches `str'. "
  (prall (helpsearch-core str) "Related symbols:\n" "\n")
  (prn)
  nil)

(def helpsearch-core (str)
  " Returns a list of symbols whose documentation matches or partly matches
    `str'. "
  (let part-match
      (let rx (re (downcase str))
         [re-match rx (downcase (coerce _ 'string))])
      (sort <
        (accum add
          (ontable k (typ d) help*
            (when (or (part-match k) (part-match typ) (part-match d) (only.part-match (source-file* k)))
              (add k)))))))

(def helpstr (name (o verbose t))
  " Returns a help string for the symbol `name'. "
  (tostring
   (let h (help* name)
     (if (no h)
         (if verbose (prn name " is not documented."))
         (with (kind  (car h)
                doc   (cadr h))
           (aand verbose ((only [prn "(from \"" _ "\")"]) (source-file* name)))
           (pr "[" kind "]" (if (is kind 'mac) " " "  "))
           (prn (if (sig name)
                    (cons name (sig name))))
           (and verbose doc (prn doc)))))))

(def fns (pfx (o test [prefix pfx _]))
  "Print sigs for macros & functions starting with pfx, or that pass test if given."
  (each f (sort < (keep test (map [string _] (keys sig))))
        (pr (helpstr (sym f) nil))))

;almkglor: err... what's this?
;(= env ($ getenv))

(defset env (x)
  (w/uniq g
    (list (list g x)
          `(env ,g)
          `(fn (val) (($ putenv) ,g val)))))

(in-package User)
(using <arc>v3)
(using <arc>v3-thread)
(using <arc>v3-scanner)
(using <arc>v3-IDEAL)

(mac % () nil)
(mac %% () nil)
(mac %%% () nil)

; used internally
(def <arc>input-history-update (expr)
  (= %%% %%
     %% %)
  (tostring (mac % () expr)))

(= ^ nil
   ^^ nil
   ^^^ nil)

(def <arc>output-history-update (val)
  (= ^^^ ^^
     ^^ ^
     ^ val))

(set current-load-file* nil)


; any logical reason I can't say (push x (if foo y z)) ?
;   eval would have to always ret 2 things, the val and where it came from
; idea: implicit tables of tables; setf empty field, becomes table
;   or should setf on a table just take n args?

; solution to the "problem" of improper lists: allow any atom as a list
;  terminator, not just nil.  means list recursion should terminate on 
;  atom rather than nil, (def empty (x) (or (atom x) (is x "")))
; table should be able to take an optional initial-value.  handle in sref.
; warn about code of form (if (= )) -- probably mean is
; warn when a fn has a parm that's already defined as a macro.
;   (def foo (after) (after))
; idea: a fn (nothing) that returns a special gensym which is ignored
;  by map, so can use map in cases when don't want all the vals
; idea: anaph macro so instead of (aand x y) say (anaph and x y)
; idea: foo.bar!baz as an abbrev for (foo bar 'baz)
;  or something a bit more semantic?
; could uniq be (def uniq () (annotate 'symbol (list 'u))) again?
; idea: use x- for (car x) and -x for (cdr x)  (but what about math -?)
; idea: get rid of strings and just use symbols
; could a string be (#\a #\b . "") ?
; better err msg when , outside of a bq
; idea: parameter (p foo) means in body foo is (pair arg)
; idea: make ('string x) equiv to (coerce x 'string) ?  or isa?
;   quoted atoms in car valuable unused semantic space
; idea: if (defun foo (x y) ...), make (foo 1) return (fn (y) (foo 1 y))
;   probably would lead to lots of errors when call with missing args
;   but would be really dense with . notation, (foo.1 2)
; or use special ssyntax for currying: (foo@1 2)
; remember, can also double; could use foo::bar to mean something
; wild idea: inline defs for repetitive code
;  same args as fn you're in
; variant of compose where first fn only applied to first arg?
;  (> (len x) y)  means (>+len x y)
; use ssyntax underscore for a var?
;  foo_bar means [foo _ bar]
;  what does foo:_:bar mean?
; matchcase
; crazy that finding the top 100 nos takes so long:
;  (let bb (n-of 1000 (rand 50)) (time10 (bestn 100 > bb)))
;  time: 2237 msec.  -> now down to 850 msec

