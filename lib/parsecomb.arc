; output from parsers is a list with the following structure:
;
; (<value> <remaining input>)
;
; * value is a string, the chars parsed
; * remaining input is a string
;
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
; in any case an index into the input string needs to be used
; to allow testing for the beginning of the input (and efficiency).


;; fundamentals

(def peek (s (o n 1))
  "Get the first n chars of the given string."
  (subseq s 0 n))

(def return (val input)
  "Signal a successful parse. Parsed chars in val, remaining input in input."
  (list val input))

(def fail ()
  "Signal a failed parse."
  nil)


;; examine a returned result

(def value (retval)
  "The parsed chars of a return value."
  (car retval))

(def reminput (retval)
  "The index to the remaining input of a return value."
  (cadr retval))


; eventually this should return an AST
(def parse (parser input (o idx 0))
  "Run the given parser on input."
  ((force parser) input)) ; force the parser if it is a delay


; all parsers should be lazy so one can build grammars thusly:
;
;   (= begin   (char "*")
;      end     (char "*")
;      emph    (seq begin (many1 content) end)  ; content not defined yet
;      content (alt emph (seq (cant-see begin) (cant-see end) any-char)))

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

;; core parsers (atoms)

(def noop (input)
  "Consume no input successfully.  If used in alternatives, use only at the end.
   Be careful with this inside many and many1."
  (return "" input))

; for internal use, core parsers
(def parse-char (input (o c) (o test [or (no c) (is _ c)]))
  "General char parsing routine. With only an input parameter parse any char.
   If c is given with no test then only parse the char c.  If a test is given
   then c is only relevant if the test uses it."
  (if (empty input)
      (fail)
      (let next (peek input)
        (if (test next)
            (return next (subseq input 1))
            (fail)))))

; .
(def any-char (input)
  "Parse any char."
  (parse-char input))

; c
(def char (c)
  "Parse the char c."
  [parse-char _ (if (isa c 'string) c (string c))])

; [^c]
(def not-char (c)
  "Parse any char except c."
  (fn (input) (parse-char input nil [isnt _ c])))

(def digit (input)
  "Parse a single decimal digit from 0 to 9."
  (parse-char input nil [<= #\0 (_ 0) #\9]))

; [abc]
; TODO accept ranges, e.g. (one-of "0-9")
(def one-of (chars)
  "Parse any char in chars."
  (fn (input) (parse-char input nil [findsubseq _ chars])))

; [^abc]
(def none-of (chars)
  "Parse any char not in chars."
  (fn (input) (parse-char input nil [no (findsubseq _ chars)])))

; (expr)?
(mac maybe (parser)
  `(alt (delay ,parser) noop))

; (expr){3}
; (expr){1,6}
(mac n-times (parser n (o m))
  `(fn (input)
     (seq-r (if (no ,m)
                (map [idfn (delay ,parser)] (range 1 ,n))
                (join (map [idfn (delay ,parser)] (range 1 ,n))
                      (map [maybe (delay ,parser)] (range (+ ,n 1) ,m))))
            input)))

(def token (s)
  "Parse the literal string s."
  (fn (input)
    (seq-r (map [char _] (($ string->list) s))
           input)))

; (?=expr)
(mac can-see (parser)
  "Look ahead.  Consumes no input."
  `(fn (input)
     (iflet result (parse (delay ,parser) input)
            (return "" input))))

; (?!expr)
(mac cant-see (parser)
  "Negative look ahead.  Consumes no input."
  `(fn (input)
     (iflet result (parse (delay ,parser) input)
            (fail)
            (return "" input))))

; is there a better way to eval an arg to a macro? yikes.
(mac mapdelay (lst)
  `(map [eval (apply delay (list _))] ',lst))

; |
; alternation, like parsec's <|>
(mac alt parsers
  `(fn (input)
     (alt-r (mapdelay ,parsers) input)))

(def alt-r (parsers input)
  (if (no parsers)
      (fail)
      (or (parse (car parsers) input)
          (alt-r (cdr parsers) input))))

; like catenation in a regex.
; parse this then that, somewhat like parsec's >>
(mac seq parsers
  (and (is 1 (len parsers)) ; allow (seq (parser parser)) or (seq parser parser)
       (acons (car parsers))
       (= parsers (car parsers)))
  `(fn (input)  ; this should *definitely* use recursion
     (seq-r (mapdelay ,parsers) input)))

(def seq-r (parsers input (o accum ""))
  (if (no parsers)
      (if (~empty accum) ; this test is bullshit, need to check properly
                         ; in case of (seq (can-see "foo")) nothing is consumed
          (return accum input))
      (iflet (parsed remaining) (parse (car parsers) input)
             (seq-r (cdr parsers) remaining (+ accum parsed)))))

; *
; repeat zero or more times (cannot fail)
(mac many (parser)
  `(fn (input)
     (many-r (delay ,parser) input)))

(def many-r (parser input (o accum ""))
  (iflet (parsed remaining) (parse parser input)
         (many-r parser remaining (+ accum parsed))
         (return accum input)))

; +
; repeat one or more times
(mac many1 (parser)
  `(fn (input)
     (many1-r (delay ,parser) input)))

(def many1-r (parser input (o accum ""))
  (iflet (parsed remaining) (parse parser input)
         (many1-r parser remaining (+ accum parsed))
         (~empty accum)
         (return accum input)))

; if an index was passed around this would work
(def beginning (input idx)
  (if (is idx 0)
      (return "" input idx)
      (fail)))

(= space  (one-of " \t\n\r")
   spaces (many1 space))
