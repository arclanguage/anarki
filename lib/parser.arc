;; (arc-tokeniser input) returns a token generator for the
;; given input. Example:
;;
;; arc> (set tkz (arc-tokeniser (instring "(foo bar)")))
;; #<procedure...>
;; arc> (tkz)
;; (left-paren 0 1)
;; arc> (tkz)
;; ("foo" 1 4)
;; arc> (tkz)
;; (space 4 5)
;; arc> (tkz)
;; ("bar" 5 8)
;; arc> (tkz)
;; (right-paren 8 9)
;; arc> (tkz)
;; nil
;;
;; (parse text) returns the arc object represented by the text.
;; example:
;;
;; arc> (parse "(def foo (plop) 'yadda \"toto\" (ccc me))")
;; (def foo (plop) 'yadda "toto" (ccc me))
;;
;; please keep lib/tests/parser-test.arc up to date with this
;;

(set named-chars  (obj 
  "#\\space"   #\space 
  "#\\newline" #\newline 
  "#\\tab"     #\tab 
  "#\\return"  #\return))

(set syntax-chars (obj 
  #\( 'left-paren 
  #\) 'right-paren 
  #\[ 'left-bracket 
  #\] 'right-bracket 
  #\' 'quote 
  #\` 'quasiquote))

(set ws-char-names (obj 
  space   " "
  newline "\\n"
  return  "\\r" 
  tab     "\\t"))

(set syntax-char-names  (obj 
  left-paren        "(" 
  left-bracket      "[" 
  right-paren       ")" 
  right-bracket     "]" 
  quote             "'" 
  quasiquote        "`" 
  unquote           "," 
  unquote-splicing  ",@"))

(set unmatched (obj 
  unmatched-left-paren        "(" 
  unmatched-left-bracket      "[" 
  unmatched-right-paren       ")" 
  unmatched-right-bracket     "]"))

(def numeric-char-spec (tok)
  (on-err (fn (ex) (err (+ "unknown char: #\\" tok)))
          (fn ()   (coerce (coerce tok 'int 8) 'char))))

(def tokenise (charlist)
  (let tok (string:rev charlist)
    (if (and (is (tok 0) #\#) (is (tok 1) #\\))
      (if (is (len tok) 2)      #\space
          (is (len tok) 3)      (tok 2)
          (aif (named-chars tok) 
               it 
               (numeric-char-spec:string:cddr:rev charlist)))
      (is (tok 0) #\;) (annotate 'comment tok)
      tok)))

(def char-terminator (ch)
  (find ch '(#\( #\) #\[ #\] #\# #\, #\` #\newline #\space #\return #\tab #\" #\')))

; shamelessly inspired from http://en.wikipedia.org/wiki/Call-with-current-continuation
(def arc-tokeniser (input) 
  "returns a generator which returns one 
   token at a time from the input"
  (let control-state nil
    (let generator (fn () (ccc control-state))
      (= control-state 
        (fn (return)
            (read-arc-tokens input 
               (fn (reader-cc tok start finish)
                   (= return (ccc (fn (cc)
                                      (= control-state cc)
                                      (return (list tok start finish)))))))))
      generator)))

(def read-arc-tokens (input caller)
  "caller is a (fn (return tok start finish) ...) where 
    - return is the return continuation,
    - tok is either the text of the token, or
      a symbol naming the token in the case of syntax, 
    - start is the index of the start of the token, 
    - finish is the index of the first character 
      immediately following the token"
  (with (state        nil
         escaping     nil 
         start        0 
         char-count   0 
         token        nil
         return       (fn (tok start finish)
                          (ccc (fn (cc) (caller cc tok start finish)))))
    (with (notify           (fn (delim)
                                (if token 
                                    (return (tokenise token) start char-count))
                                (wipe token)
                                (if delim 
                                    (return delim char-count (+ char-count 1))))
           append-to-token  (fn (ch)
                                (if (no token) (= start char-count))
                                (push ch token)))
      ((afn (ch)
        (if (is state 'reading-comment)
                                (if (is ch #\newline)   (wipe state)
                                    ch                  (do (append-to-token ch) 
                                                            (= ch 'ignore))))

        (if (is state 'reading-unquote)
                                (do (wipe state)
                                    (if (is ch #\@)
                                        (do (notify 'unquote-splicing) 
                                            (= ch 'ignore))
                                        (notify 'unquote))))

        (if (is state 'reading-char)
                                (do (if ch 
                                        (append-to-token ch)
                                        (wipe state))
                                    (if (and (> (len token) 3) (char-terminator ch))
                                        (do (wipe state) (pop token))
                                        (= ch 'ignore))))

        (if (is state 'reading-string)
                                (do (if ch (append-to-token ch))
                                    (if escaping
                                        (wipe escaping)
                                        (if (is ch #\") (wipe state)
                                            (is ch #\\) (do (assert escaping)))))
            (and (no token) (is ch #\#))
                                (do (append-to-token ch)
                                    (= state 'reading-char))
            (and (no token) (is ch #\;))
                                (do (append-to-token ch)
                                    (= state 'reading-comment))
            (syntax-chars ch)   (notify (syntax-chars ch))
            (is ch #\,)         (do (notify nil)
                                    (= state 'reading-unquote))
            (is ch #\newline)   (notify 'newline)
            (is ch #\space)     (notify 'space)
            (is ch #\")         (do (notify nil)
                                    (= state 'reading-string)
                                    (append-to-token ch))
            (no ch)             (notify nil)
            (no:is ch 'ignore)  (append-to-token ch))

        (if ch                  (do (++ char-count) 
                                    (self (readc input))))
      ) (readc input)))))

(def parse (text)
  "parses the given text (input or string) 
   and returns the corresponding arc object"
  (read-form (arc-tokeniser (if (isa text 'string) (instring text) text))))

(def next-form (tok token-generator)
  (if 
    (is tok 'left-paren)          (read-list token-generator 'right-paren)
    (is tok 'left-bracket)        (list 'fn '(_) (read-list token-generator 'right-bracket))
    (or (is tok 'quasiquote) (is tok 'quote) (is tok 'unquote) (is tok 'unquote-splicing))
                                  (list tok (read-form token-generator))
    (ws-char-names tok)           nil
                                  (read-atom tok)))

(def read-form (token-generator)
  (next-form (car:token-generator) token-generator))

(def read-string-tok (tok-chars)
  (string:rev (accum s
    ((afn (chs escaping)
      (let ch (car chs)
        (if escaping 
            (do
              (case ch
                #\\ (s ch)
                #\" (s ch)
                #\n (s #\newline)
                #\r (s #\return)
                #\t (s #\tab))
              (self (cdr chs) nil))
            (case ch
              #\\ (self (cdr chs) t)
              #\" nil
                  (do (s ch) 
                      (self (cdr chs) nil)))))
    ) (cdr tok-chars) nil))))

(def read-atom (tok)
  (if (is (type tok) 'char)     tok
      (is (type tok) 'comment)  tok
      (and (is #\" (tok 0)) (is #\" (tok (- (len tok) 1))))
                                (read-string-tok (coerce tok 'cons))
                                (on-err (fn (ex) (sym tok))
                                        (fn ()   (coerce tok 'int)))))

(def read-list (token-generator terminator)
  (let toklist nil
    ((afn (tok)
      (if (no:is tok terminator)
        (do 
          (aif (next-form tok token-generator) (push it toklist)) 
          (self:car:token-generator))
      )) (car:token-generator))
    (rev toklist)))

(def link-parens (right left)
  (if left
    (do
      (= (right 1) (left 1))
      (= (left 2) (right 2)))
    (scar right (sym (+ "unmatched-" (string (car right)))))))

(def index-source (text)
  "returns a list of (tok start finish) where start and finish
   represent where the token occurs in the text, except for 
   parens and brackets, where start and end are used for
   matching. Used by welder for colourising"
  (with (result nil parens nil brackets nil)
    (read-arc-tokens (instring text)
                     (fn (return . args)
                        (push args result)
                        (if (is (car args) 'left-paren)     (push args parens)
                            (is (car args) 'left-bracket)   (push args brackets)
                            (ws-char-names (car args))      (pop result)
                            (is (car args) 'right-bracket)  (link-parens args (pop brackets))
                            (is (car args) 'right-paren)    (link-parens args (pop parens)))
                        (return nil)))
    (each p parens   (scar p 'unmatched-left-paren))
    (each p brackets (scar p 'unmatched-left-bracket))
    (rev result)))
