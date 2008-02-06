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

; this could return something somewhat AST-like
(def parse (parser input (o idx 0))
  "Run the given parser on input."
  (parser input))


;; core parsers

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
(def maybe (parser)
  (alt parser noop))

; (expr){3}
; (expr){1,6}
(def n-times (parser n (o m))
  (apply seq (if (no m)
                 (map [idfn parser] (range 1 n))
                 (join (map [idfn parser] (range 1 n))
                       (map [maybe parser] (range (+ n 1) m))))))

(def token (s)
  "Parse the literal string s."
  (apply seq (map [char _] (($ string->list) s))))

; (?=expr)
(def can-see (parser)
  "Look ahead.  Consumes no input."
  (fn (input)
    (let result (parser input)
      (if (success result)
          (return "" input)
          result))))

; (?!expr)
(def cant-see (parser)
  "Negative look ahead.  Consumes no input."
  (fn (input)
    (let result (parser input)
      (if (success result)
          (fail)
          (return "" input)))))

; |
; alternation, like parsec's <|>
(def alt choices
  (fn (input)
    (with (result 'notnil i 0)
      (while (and (< i (len choices))
                  (no result))
        (= result ((choices i) input))
        (++ i))
      result)))

; like catenation in a regex.
; parse this then that, somewhat like parsec's >>
(def seq parsers
  (fn (input)
    (with (result    nil
           parsed    ""
           remaining input)
      (while (and (no (empty parsers))
                  (= result ((car parsers) remaining)))
        (= parsed    (+ parsed (value result))
           remaining (reminput result)
           parsers   (cdr parsers)))
      (if (and (empty parsers)
               result)
          (return parsed remaining)))))

; *
; repeat zero or more times (cannot fail)
(def many (parser)
  (fn (input)
    (with (result    nil
           parsed    ""
           remaining input)
      (while (= result (parser remaining))
        (= parsed    (+ parsed (value result))
           remaining (reminput result)))
      (return parsed remaining))))

; +
; repeat one or more times
(def many1 (parser)
  (fn (input)
    (with (result    nil
           parsed    ""
           remaining input)
      (while (= result (parser remaining))
        (= parsed    (+ parsed (value result))
           remaining (reminput result)))
      (if (> (len parsed) 0) ; success if we consumed any input
          (return parsed remaining)))))

; if an index was passed around this would work
(def beginning (input idx)
  (if (is idx 0)
      (return "" input idx)
      (fail)))

(= space  (one-of " \t\n\r")
   spaces (many1 space))
