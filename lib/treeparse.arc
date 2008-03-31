; treeparse.arc -- born on 2008/02/27
;
; A parser combinator library for parsing lists and trees.
; Loosely based on parsecomb.arc and Parsec.
;
; Parsers are functions that take a list as input and return a list
; with the following structure:
;
;     (<parsed list> <remaining list> <actions>)
;
; Parsers return nil on failure.
;
; To parse a scheme-ish binary number
;    e.g. ("#" "b" 0 0 0 1 0 1 0 1 0)
; (= bin-digit    (alt 0 1)
;    binary (seq "#" "b" (many1 bin-digit)))
; then call (parse binary <input string>)
; or just (binary <input string>)
;
; Use `sem' to embed semantics.
; Use `filt' to embed filters.
;
; Examples in "lib/treeparse-examples.arc"

(require "lib/tconc.arc")

(mac delay-parser (p)
  "Delay evaluation of a parser, in case it is not yet defined."
  (let remaining (uniq)
    `(fn (,remaining)
       (parse ,p ,remaining))))

(mac assign-parser (var p)
  "Create a parser which assigns the parsed value to the specified variable.
The variable is in the same scope as the assign-parser definition."
  (w/uniq (remaining retval)
    `(fn (,remaining)
       (let ,retval (parse ,p ,remaining)
         ; have to copy: combinators are destructive
         ; on the parsed values
         (when ,retval (= ,var (copy:car ,retval)))
         ,retval))))

(def return (val remaining (o actions nil))
  "Signal a successful parse."
  (list val remaining actions))

(def parse-all (parser input)
  "Calls parse, returning the parsed list only if the entire input was
parsed. Otherwise returns nil and prints an error. Semantics are
executed on success."
  (awhen (parse parser input)
    (if (no:it 1) (do (carry-out it) (it 0))
                  (do (pr "Parse error: extra tokens '")
                      (prn (it 1))
                      nil))))

(def parse (parser remaining)
  "Apply parser (or literal, or list) to input."
  (if (isa parser 'fn) (parser remaining)
      (acons parser) (parse-list parser remaining)
      (parse (lit parser) remaining)))

(def parse-list (parsers li)
  (when (and li (alist li) (alist (car li)))
    (iflet (parsed remaining actions) (seq-r parsers (car li)
                                              (tconc-new) nil)
           (unless remaining (return (list parsed)
                                     (cdr li) actions)))))

(def lit (a)
  "Creates a parser that matches a literal. You shouldn't need to
call this directly, `parse' should wrap up literals for you."
  (fn (remaining)
    (when (and (acons remaining) (iso a (car remaining)))
      (return (list (car remaining)) (cdr remaining)))))

(def nil-lit (a)
  "Creates a parser that matches a literal; however, it discards
the matched literal, returning the same value as 'nothing."
  (fn (remaining)
   (when (and (acons remaining) (iso a (car remaining)))
      (return nil (cdr remaining)))))

(def seq parsers
  "Applies parsers in sequential order."
  (seq-l parsers))

(def seq-l (parsers)
  "Applies the list of parsers in sequential order"
  (fn (remaining)
    (seq-r parsers remaining (tconc-new) nil)))

(def seq-r (parsers li acc act-acc)
  (if (no parsers) (return (car acc) li act-acc)
      (iflet (parsed remaining actions) (parse (car parsers) li)
             (seq-r (cdr parsers) remaining 
                    (lconc acc parsed)
                    (if actions (join act-acc actions)
                                act-acc)))))

(def nil-seq parsers
  "Applies parsers in sequential order; the results of the parsers
are ignored."
  (nil-seq-l parsers))

(def nil-seq-l (parsers)
  "Applies the list of  parsers in sequential order; the results of
the parsers are ignored."
  (fn (remaining)
    (nil-seq-r parsers remaining)))

(def nil-seq-r (parsers li)
  (if (no parsers) (return nil li)
      (iflet (_ remaining __) (parse (car parsers) li)
             (nil-seq-r (cdr parsers) remaining))))

(def alt parsers
  "Alternatives, like Parsec's <|>."
  (alt-l parsers))

(def alt-l (parsers)
  "A list of alternatives, like Parsec's <|>."
  (fn (remaining) (alt-r parsers remaining)))

(def alt-r (parsers remaining)
  (if (no parsers)
      nil
      (aif (parse (car parsers) remaining)
           it
           (alt-r (cdr parsers) remaining))))

(def nothing (remaining)
  "A parser that consumes nothing."
  (return nil remaining))

(def at-end (remaining)
  "A parser that succeeds only if input is empty."
  (unless remaining (return nil nil)))

(def anything (remaining)
  "A parser that consumes one token."
  (when (acons remaining)
    (return (list (car remaining)) (cdr remaining))))

(def anything-but parsers
  "Anything that 'parsers' will not accept."
  (seq (cant-see (apply alt parsers)) anything))

(def maybe (parser)
  "Parser appears once, or not."
  (alt parser nothing))

(def cant-see (parser)
  "Parser does not appear next in the input stream."
  (fn (remaining)
    (if (parse parser remaining) nil
        (return nil remaining))))

(def many (parser)
  "Parser is repeated zero or more times."
  (fn (remaining) (many-r parser remaining (tconc-new) nil)))

(def many-r (parser li acc act-acc)
  (iflet (parsed remaining actions) (parse parser li)
         (many-r parser remaining
                 (lconc acc parsed)
                 (if actions (join act-acc actions) act-acc))
         (return (car acc) li act-acc)))

(def many1 (parser)
  "Parser is repeated one or more times."
  (seq parser (many parser)))

(def many2 (parser)
  "Parser is repeated two or more times."
  (seq parser (many1 parser)))

(def nil-many (parser)
  "Parser is repeated zero or more times; their return value is
ignored."
  (fn (remaining) (nil-many-r parser remaining)))

(def nil-many-r (parser li)
  (iflet (parsed remaining actions) (parse parser li)
         (nil-many-r parser remaining)
         (return nil li)))

(def nil-many1 (parser)
  "Parser is repeated one or more times."
  (nil-seq parser (nil-many parser)))

(def nil-many2 (parser)
  "Parser is repeated two or more times."
  (nil-seq parser (nil-many1 parser)))

(def pred (test parser)
  "Create a parser that succeeds if `parser' succeeds and its output
passes `test'."
  (fn (remaining)
    (awhen (parse parser remaining)
      (and (test (car it)) it))))

(def sem (fun parser)
  "Attach semantics to a parser."
  (fn (remaining)
    (iflet (parsed remaining actions) (parse parser remaining)
           (return parsed remaining
                   (join actions
                         (let parsed (copy parsed)
                           (list (fn () (fun parsed)))))))))

(def filt (fun parser)
  "Attach filter to a parser."
  (fn (remaining)
    (iflet (parsed remaining actions) (parse parser remaining)
           (return (fun parsed) remaining actions))))

(def filtcar (fun parser)
  "Like filt, but only operates on the car of the input.
  (filtcar foo p) == (filt foo:car p)."
  (filt fun:car parser))

(def filt-map (fun parser)
  "Like filt, but applies the function to each element of the
   parsed result."
  (fn (remaining)
    (iflet (parsed remaining actions) (parse parser remaining)
           (do
             ((afn (p)
                (when p
                  (zap fun (car p))
                  (self (cdr p))))
              parsed)
             (return parsed remaining actions)))))

(def carry-out (result)
  "Execute the semantics of a parser result."
  (each f (result 2) (f)))
