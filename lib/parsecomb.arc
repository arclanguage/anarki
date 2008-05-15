; Parsers are functions that accept an input string and an index into said
; string.  Output from parsers is a list with the following structure:
;
;     (<parsed chars> <new index>)
;
; If the parse failed the parser returns nil.
; Most of the time char means a string of length 1.
;
; an example that should parse a scheme-ish binary number
;    e.g. #b000101010
;(= bin-digit    (one-of "01")
;   binary (seq (char "#") (char "b") (many1 bin-digit)))
; then call (parse binary <input string>)
; or just (binary <input string>) as parse does nothing useful (yet)

; it would be nice for parse to return an object-like closure
; that can step through the parse token by token.


;; fundamentals

(def peek (s i (o n 1))
  "Get the first n chars of the given string."
  (cut s i (+ i n)))

(def return (val i)
  "Signal a successful parse. Parsed chars in val, new index in i."
  (list val i))

(def value (retval)
  "Return the chars parsed."
  (car retval))

(def newi (retval)
  "The new index to the remaining input."
  (cadr retval))


; eventually this should return an AST
(def parse (parser s (o i 0))
  "Run the given parser on s starting from i."
  ((force parser) s i)) ; force the parser if it is a delay


; Parsers are lazy so we can build grammars thusly:
;
;   (= ebegin  (char "*")
;      eend    (char "*")
;      emph    (seq ebegin (many1 content) eend)  ; content not defined yet
;      content (alt emph (seq (cant-see ebegin) (cant-see eend) any-char)))

; this idea is from On Lisp, seems to work well
(mac delay (expr)
  (if (isdelay expr)    ; don't nest delays
      expr
      `'(delay ,expr)))

(def isdelay (x)
  (and (acons x) (is (car x) 'delay)))

 (def force (x)
   (if (isdelay x)
       (eval (last x))
       x))

; arc's atend function is not what we want, it's true even when i points
; to the last char, not past it.  we need something like \z in regular expressions.
(def reallyatend (i s)
  (>= i (len s)))

;; core parsers (atoms)

(def start (s i)
  "Match the beginning of a string."
  (if (is i 0)
      (return "" i)))

(def end (s i)
  "Match the end of a string."
  (if (reallyatend i s)
      (return "" i)))

(def noop (s i)
  "Consume no input successfully.  If used in alternatives, use only at the end.
   Be careful with this inside many and many1."
  (return "" i))

; for internal use, core parsers
(def parse-char (s i (o c) (o test [or (no c) (is _ c)]))
  "General char parsing routine. With only an input parameter parse any char.
   If c is given with no test then only parse the char c.  If a test is given
   then c is only relevant if the test uses it."
  (if (~reallyatend i s)
      (let next (peek s i)
        (if (test next)
            (return next (+ i 1))))))

; .
(def any-char (s i)
  "Parse any char."
  (parse-char s i))

; c
(def char (c)
  "Parse the char c."
  (fn (s i) (parse-char s i (if (isa c 'string) c (string c)))))

; [^c]
(def not-char (c)
  "Parse any char except c."
  (fn (s i) (parse-char s i nil [isnt _ c])))

(def digit (s i)
  "Parse a single decimal digit from 0 to 9."
  (parse-char s i nil [<= #\0 (_ 0) #\9]))

; [abc]
; TODO accept ranges, e.g. (one-of "0-9")
(def one-of (chars)
  "Parse any char in chars."
  (fn (s i) (parse-char s i nil [findsubseq _ chars])))

; [^abc]
(def none-of (chars)
  "Parse any char not in chars."
  (fn (s i) (parse-char s i nil [~findsubseq _ chars])))

; (expr)?
(mac maybe (parser)
  `(alt (delay ,parser) noop))

; (expr){3}
; (expr){1,6}
(mac n-times (parser n (o m))
  `(fn (s i)
     (seq-r (if (no ,m)
                (map [delay ,parser] (range 1 ,n))
                (join (map [delay ,parser] (range 1 ,n))
                      (map [maybe ,parser] (range (+ ,n 1) ,m))))
            s i)))

; parse the sequence of chars given in order using seq-r.
; join the list of chars from seq-r so it looks like we parsed them as a unit.
(def token (t)
  "Parse the literal string s."
  (fn (s i)
    (iflet (parsed newi) (seq-r (map char (coerce t 'cons)) s i)
           (return (apply string parsed) newi))))

; (?=expr)
(mac can-see (parser)
  "Look ahead.  Consumes no input."
  `(fn (s i)
     (iflet result (parse (delay ,parser) s i)
            (return "" i))))

; (?!expr)
(mac cant-see (parser)
  "Negative look ahead.  Consumes no input."
  `(fn (s i)
     (iflet result (parse (delay ,parser) s i)
            nil
            (return "" i))))

; is there a better way to eval an arg to a macro? yikes.
(mac mapdelay (lst)
  `(map [eval:apply (rep delay) (list _)] ',lst))

(def mkparser (x)
  (if (isa x 'string) `(token ,x) x))

; |
; alternation, like parsec's <|>
(mac alt parsers
  `(fn (s i)
     (alt-r (mapdelay ,(map mkparser parsers)) s i)))

(def alt-r (parsers s i)
  (and parsers
       (or (parse (car parsers) s i)
           (alt-r (cdr parsers) s i))))

; like catenation in a regex.
; parse this then that, somewhat like parsec's >>
(mac seq parsers
  `(fn (s i)
     (seq-r (mapdelay ,(map mkparser parsers)) s i)))

(def seq-r (parsers s i (o acc))
  (if (no parsers)
      (and acc (return (rev acc) i))
      (iflet (parsed newi) (parse (car parsers) s i)
             (seq-r (cdr parsers) s newi (cons parsed acc)))))

; *
; repeat zero or more times (cannot fail)
(mac many (parser)
  `(fn (s i)
     (many-r (delay ,parser) s i)))

(def many-r (parser s i (o acc ""))
  (iflet (parsed newi) (parse parser s i)
         (many-r parser s newi (+ acc parsed))
         (return acc newi)))

; +
; repeat one or more times
(mac many1 (parser)
  `(fn (s i)
     (many1-r (delay ,parser) s i)))

(def many1-r (parser s i (o acc ""))
  (iflet (parsed newi) (parse parser s i)
          (many1-r parser s newi (+ acc parsed))
         (~empty acc)
          (return acc i)))

; need something like parsec's sepBy and endBy parsers

(= space  (one-of " \t\n\r")
   spaces (many1 space))
