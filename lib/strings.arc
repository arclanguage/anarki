; Matching.  Spun off 29 Jul 06.

; arc> (tostring (writec (coerce 133 'char)))
;
;> (define ss (open-output-string))
;> (write-char (integer->char 133) ss)
;> (get-output-string ss)
;"\u0085"

(def tokens (s (o sep whitec))
"Breaks up the string 's' at characters matching the predicate 'sep'
(whitespace by default). Continuous runs of such characters count as a single
separation; no empty strings are returned."
  (let test testify.sep
    (rev:map [coerce _ 'string]
             (map rev
                  (loop (cs  (coerce s 'cons)
                         toks  nil
                         tok  nil)
                    (if no.cs
                          (consif tok toks)
                        (test car.cs)
                          (recur cdr.cs (consif tok toks) nil)
                        :else
                          (recur cdr.cs toks (cons car.cs tok))))))))

; maybe promote to arc.arc, but if so include a list clause

(def positions (test seq)
"Returns all the indices in 'seq' at which 'test' passes."
  (accum yield
    (let f testify.test
      (forlen i seq
        (if (f seq.i)
          (yield i))))))

(def lines (s)
"Breaks up a multi-line string into lines, respecting either newlines or CRLF
sequences."
  (map [rem #\return _]
       (slices s #\newline)))

(def slices (s test)
"Like [[tokens]] but creates a new string at every character matching 'test',
creating empty strings as necessary."
  (accum yield
    (loop (p  -1
           ps  (positions test s))
      (if no.ps
        (yield (cut s (+ p 1)))
        (do (yield (cut s (+ p 1) car.ps))
            (recur car.ps cdr.ps))))))

(def nonascii (s)
  (~is len.s (len:utf-8-bytes s)))

; > (require (lib "uri-codec.ss" "net"))
;> (form-urlencoded-decode "x%ce%bbx")
;"xλx"

; first byte: 0-7F, 1 char; c2-df 2; e0-ef 3, f0-f4 4.

; Fixed for utf8 by pc.

(def urldecode (s)
"Reverse [[urlencode]], replacing runs of encoded %sequences with
corresponding (potentially multibyte) characters."
 (tostring
  (forlen i s
    (caselet c (s i)
      #\+ (writec #\space)
      #\% (do (when (> (- len.s i) 2)
                (writeb (int (cut s (+ i 1) (+ i 3)) 16)))
              (++ i 2))
          (writec c)))))

; http://stackoverflow.com/questions/9635661/encodeuricomponent-algorithm-source-code
(def urlencode (s)
"Encode string 's' using only characters permitted in urls according to the http spec.
Should behave just like javascript's encodeURIComponent."
  (tostring
    (each code-point (utf-8-bytes s)
      (let c (coerce code-point 'char)
        (if (or (>= code-point 128)
                (~alphadig c)
                (in c #\! #\' #\( #\) #\* #\- #\. #\_ #\~))
          (urlencode-char code-point)
          (writec c))))))

(def urlencode-char (i)
  (writec #\%)
  (if (< i 16) (writec #\0))
  (pr (coerce i 'string 16)))

(def posmatch (pat seq (o start 0))
"Returns the first index after 'start' where substring 'pat' is found in 'seq'."
  (catch
    (if (isa pat 'fn)
      (up i start len.seq
        (when (pat seq.i)
          (throw i)))
      (up i start (- len.seq (- len.pat 1))
        (when (headmatch pat seq i)
          (throw i))))
    nil))

(def headmatch (pat seq (o start 0))
"Does 'seq' contain 'pat' at index 'start'?"
  (when (>= len.seq
            (+ start len.pat))
    (loop (i 0 j start)
      (or (is i len.pat)
          (when (iso pat.i seq.j)
            (recur inc.i inc.j))))))

(def endmatch (pat seq)
"Does 'seq' end with 'pat'?"
  (headmatch rev.pat rev.seq))

(defextend rev (x)  (isa x 'string)
  (as string (rev:as cons x)))

(def begins (seq pat (o start 0))
"Like [[headmatch]] but with 'seq' and 'pat' reversed."
  (headmatch pat seq start))

(defextend subst (old new seq) (isa seq 'string)
  (tostring
    (forlen i seq
      (if (and (< i (- len.seq len.old -1))
               (headmatch old seq i))
        (do (++ i (- len.old 1))
            (pr new))
        (pr seq.i)))))

(def multisubst (pairs seq)
"For each (old new) pair in 'pairs', substitute 'old' with 'new' in 'seq'."
  (tostring
    (forlen i seq
      (iflet (old new) (find [begins seq (car _) i] pairs)
        (do (++ i (- len.old 1))
            (pr new))
        (pr seq.i)))))

(def blank (s)
"Is 's' empty or all whitespace?"
  (~find ~whitec s))

(def nonblank (s)
"Returns string 's' unless it's blank."
  (unless blank.s s))

(def trim (s (o where 'both) (o test whitec))
"Strips out characters matching 'test' from front/start/begin of 's',
back/finish/end, or both."
  (withs (f   testify.test
          p1  (pos ~f s))
    (if p1
      (cut s
           (if (in where 'front 'start 'begin 'both) p1 0)
           (when (in where 'back 'finish 'end 'both)
             (let i (- len.s 1)
               (while (and (> i p1) (f s.i))
                 (-- i))
               (+ i 1))))
      "")))

(def num (n (o digits 2) (o trail-zeros nil) (o init-zero nil))
"Formats 'n' as a string with the appropriate 'digits' of precision, padding
trailing zeros and an initial zero before the decimal as desired."
  (withs (comma
          (fn (i)
            (rev:string:intersperse #\,
                                    (tuples (rev:as cons string.i)
                                            3)))
          abrep
          (let a abs.n
            (if (< digits 1)
                 (comma roundup.a)
                (exact a)
                 (string comma.a
                         (when (and trail-zeros (> digits 0))
                           (string "." (newstring digits #\0))))
                 (withs (d (expt 10 digits)
                         m (/ (roundup (* a d)) d)
                         i trunc.m
                         r (abs:trunc (- (* m d) (* i d))))
                   (+ (if (is i 0)
                        (if (or init-zero (is r 0)) "0" "")
                        comma.i)
                      (withs (rest   string.r
                              padded (+ (newstring (- digits len.rest) #\0)
                                        rest)
                              final  (if trail-zeros
                                       padded
                                       (trim padded 'end #\0)))
                        (string (unless empty.final ".")
                                final)))))))
    (if (and (< n 0) (find [and (digit _) (isnt _ #\0)] abrep))
      (+ "-" abrep)
      abrep)))

(def joinstr (lst (o glue " "))
  (string:intersperse glue lst))

; by Andrew Wilcox
(def begins-rest (pattern s)
  (if (begins s pattern)
    (cut s len.pattern)))

; English

(def pluralize (n str (o plural-form))
"Returns plural form of 'str' if 'n' is not 1."
  (if (is n 1)
    str
    (or plural-form
        (string str "s"))))

; Import Scheme's regular expressions
(= re $.regexp)
(def re-match (rx s (o start 0) (o end nil) (o output-port $.#f) (o input-prefix ($.bytes)))
     ($.regexp-match
       rx
       s
       start
       (if end end
	       len.s)
       output-port
       input-prefix))
(def pre-match (px s (o start 0) (o end nil) (o output-port $.#f) (o input-prefix ($.bytes)))
     ($.regexp-match
       (if $.string?.px $.pregexp.px
	   $.bytes?.px	$.byte-pregexp.px
	   		px)
       s
       start
       (if end end
	       len.s)
       output-port
       input-prefix))
(= re-match? [no:no:re-match _1 _2])
(= re-pos $.regexp-match-positions)
(= re-subst $.regexp-replace)

(def plural (n x (o plural-form))
"Returns a phrase like \"3 apples\", [[pluralize]]ing depending on 'n'."
  (string n #\space (pluralize n x plural-form)))

(def capitalize (str)
  (if empty.str
    str
    (+ (upcase str.0) (cut str 1))))

(def chomp (s)
  (if (iso (s (- len.s 1))
           #\newline)
    (cut s 0 (- len.s 1))
    s))

; http://www.eki.ee/letter/chardata.cgi?HTML4=1
; http://jrgraphix.net/research/unicode_blocks.php?block=1
; http://home.tiscali.nl/t876506/utf8tbl.html
; http://www.fileformat.info/info/unicode/block/latin_supplement/utf8test.htm
; http://en.wikipedia.org/wiki/Utf-8
; http://unicode.org/charts/charindex2.html
