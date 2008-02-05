; output from parsers is a list with the following structure:
;
; (<success or fail> <value> <remaining input>)
;
; * success or fail is boolean
; * value is a string, usually the chars parsed
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


;; fundamentals

(def peek (s (o n 1))
  "Get the first n chars of the given string."
  (subseq s 0 n))

(def return (val input)
  "Signal a successful parse. Parsed chars in val, remaining input in input."
  (list t val input))

(def fail (input)
  "Signal a failed parse."
  (list nil "" input))


;; examine a returned result

(def success (retval)
  "Test a return value for success."
  (car retval))

(def failed (retval)
  "Test a return value for failure."
  (no (car retval)))

(def value (retval)
  "The parsed chars of a return value."
  (cadr retval))

(def reminput (retval)
  "The remaining input of a return value."
  (last retval))

; this could return something somewhat AST-like
(def parse (parser input)
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
      (fail input)
      (let next (peek input)
        (if (test next)
            (return next (subseq input 1))
            (fail input)))))

; .
(def any-char (input)
  "Parse any char."
  (parse-char input))

; c
(def char (c)
  "Parse the char c."
  [parse-char _ c])

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

; (expr){3}
; (expr){1,6}
(def n-times (parser n (o m))
  (apply seq (if (no m)
                 (map [idfn parser] (range 1 n))
                 (join (map [idfn parser] (range 1 n))
                       (map [alt parser noop] (range (+ n 1) m))))))

; |
; alternation, like parsec's <|>
(def alt choices
  (fn (input)
    (with (result nil i 0)
      (while (and (< i (len choices))
                  (or (no result)
                      (failed result)))
        (= result ((choices i) input))
        (++ i))
      (or result (fail input)))))

; like catenation in a regex.
; parse this then that, somewhat like parsec's >>
(def seq parsers
  (fn (input)
    (with (result    (noop input)
           remaining input
           accum     "")
      (while (and (no (empty parsers))
                  (success result))
        (= result    ((car parsers) remaining)
           remaining (reminput result)
           accum     (+ accum (value result))
           parsers   (cdr parsers)))
      (if (and (empty parsers)
               (success result))
          (return accum remaining)
          (fail input)))))

; *
; repeat zero or more times (cannot fail)
(def many (parser)
  (fn (input)
    (with (result    (noop input)
           accum     ""
           remaining input)
      (while (and (no (empty remaining))
                  (success result))
        (= result    (parser remaining)
           accum     (+ accum (value result))
           remaining (reminput result)))
      (return accum remaining))))

; +
; repeat one or more times
(def many1 (parser)
  (fn (input)
    (with (result    (noop input)
           accum     ""
           remaining input)
      (while (and (no (empty remaining))
                  (success result))
        (= result    (parser remaining)
           accum     (+ accum (value result))
           remaining (reminput result)))
      (if (> (len accum) 0) ; success if we consumed any input
          (return accum remaining)
          (fail input)))))

; rewind if the parser fails, consuming nothing
(def try (parser)
  (fn (input)
    (let result (parser input)
      (if (success result)
          result
          (fail input)))))

(= space  (one-of " \t\n\r")
   spaces (many1 space))

