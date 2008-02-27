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
; If the parse failed the parser returns nil.
;
; An example that should parse a scheme-ish binary number
;    e.g. ("#" "b" 0 0 0 1 0 1 0 1 0)
; (= bin-digit    (alt 0 1)
;    binary (seq "#" "b" (many1 bin-digit)))
; then call (parse binary <input string>)
; or just (binary <input string>)
;
; Use `sem' to embed semantics.
; Use `filt' to embed filters.
;
; More examples soon to come.


(def return (val rem (o actions nil))
  "Signal a successful parse. Parsed list in val, remaining tokens in rem."
  (list val rem actions))

(def parse (parser rem)
  "Apply parser (or literal, or list) to input."
  (if (isa parser 'fn) (parser rem)
      (acons parser) (parse-list parser rem)
      (parse (lit parser) rem)))

(def parse-list (parsers li)
  (when (alist li)
    (iflet (parsed rem actions) (seq-r parsers (car li) nil nil)
           (unless rem (return (list parsed)
                               (cdr li) actions)))))

(def lit (a)
  "Creates a parser that matches a literal."
  (fn (rem)
    (when (and (acons rem) (iso a (car rem)))
      (return (list (car rem)) (cdr rem)))))

(mac seq parsers
  "Applies parsers in sequential order."
  (let rem (uniq)
    `(fn (,rem)
       (seq-r (list ,@parsers) ,rem nil nil))))

(def seq-r (parsers li acc act-acc)
  (if (no parsers) (return acc li act-acc)
      (iflet (parsed rem actions) (parse (car parsers) li)
             (seq-r (cdr parsers) rem 
                    (join acc parsed)
                    (join act-acc actions)))))

(mac alt parsers
  "Alternatives, like Parsec's <|>."
  (let rem (uniq)
    `(fn (,rem)
       (alt-r (list ,@parsers) ,rem))))

(def alt-r (parsers rem)
  (if (no parsers) nil
      (or (parse (car parsers) rem)
          (alt-r (cdr parsers) rem))))

(def nothing (rem)
  "A parser that consumes nothing."
  (return nil rem))

(def anything (rem)
  "A parser that consumes one token."
  (when (acons rem)
    (return (list (car rem)) (cdr rem))))

(def maybe (parser)
  "Parser appears once, or not."
  (alt parser nothing))

(mac cant-see (parser)
  "Parser does not appear next in the input stream."
  `(fn (rem)
     (if (parse ,parser rem) nil
         (return nil rem))))

(mac many (parser)
  "Parser is repeated zero or more times."
  (let rem (uniq)
    `(fn (,rem) (many-r ,parser ,rem nil nil))))

(def many-r (parser li acc act-acc)
  (iflet (parsed rem actions) (parse parser li)
         (many-r parser rem
                 (join acc parsed) 
                 (join act-acc actions))
         (return acc li act-acc)))

(mac many1 (parser)
  "Parser is repeated one or more times."
  (let rem (uniq)
    `(fn (,rem)
       (parse (seq ,parser (many ,parser)) ,rem))))

(def sem (fun parser)
  "Attach semantics to a parser."
  (fn (rem)
    (iflet (parsed rem actions) (parse parser rem)
      (return parsed rem
              (join actions
                    (list (fn () (fun parsed))))))))

(def filt (fun parser)
  "Attach filter to a parser."
  (fn (rem)
    (iflet (parsed rem actions) (parse parser rem)
      (return (fun parsed) rem actions))))

(def carry-out (result)
  "Execute the semantics of a parser result."
  (each f (result 2) (f)))
