(set syntax-chars (obj
  #\( 'left-paren
  #\) 'right-paren
  #\[ 'left-bracket
  #\] 'right-bracket
  #\' 'quote
  #\` 'quasiquote))

(set character-constants  (obj
  "#\\space"   #\space
  "#\\newline" #\newline
  "#\\tab"     #\tab
  "#\\return"  #\return))

(def whitespace? (ch)
  (in ch #\space #\newline #\tab #\return))

(def make-character (token)
  (if (is (len token) 3)
      (token 2)
      character-constants.token
      character-constants.token
      (on-err (fn (ex) (err (string "unknown char: " token)))
              (fn ()   (coerce (coerce (cut token 2) 'int 8) 'char)))))

(def tokenise ((token start kind))
  (if (isa token 'sym)
      (list 'syntax token start (+ start 1))
      (let tokend (+ start len.token)
        (= token (string:rev token))
        (if kind
            (list kind token start tokend)
            (let c0 (token 0)
              (if whitespace?.c0   (list 'whitespace token start tokend)
                  (is c0 #\#)      (list 'char (make-character token) start tokend)
                  (is c0 #\;)      (list 'comment token start tokend)
                  (on-err (fn (ex) (list 'sym 
                                         (if (is token "||") '|| (sym token)) 
                                         start
                                         tokend))
                            (fn () (list 'int (coerce token 'int) start tokend)))))))))

(def char-terminator (ch)
  (or syntax-chars.ch whitespace?.ch (in ch #\" #\, #\#)))

(mac fpush (x xs)
  `(set ,xs (cons ,x ,xs)))

(mac fpop (xs)
  (w/uniq gp
  `(let ,gp (car ,xs)
     (set ,xs (cdr ,xs))
     ,gp)))

(mac fwipe (place)
  `(set ,place nil))

(def arc-tokeniser (char-stream)
  (with ((default in-string interpolating escaping in-character in-comment in-atom in-unquote) nil
         (token state token-queue) nil
         (nextc enq-token enq-token1 enq/switch0 enq/switch tokenator add-to-token) nil
         lines       1
         char-count  0
         token-start 0)

    (= nextc        (fn ()
                        (set char-count (+ char-count 1))
                        (readc char-stream))
       enq-token    (fn ((o token-kind nil))
                        (when token 
                          (fpush (list token (- token-start 1) token-kind) token-queue)
                          (fwipe token)))
       enq-token1   (fn (another (o token-kind nil))
                        (enq-token token-kind)
                        (fpush (list another (- char-count 1) nil) token-queue))
       enq/switch0  (fn (new-state)
                        (enq-token)
                        (set state new-state))
       enq/switch   (fn (another-tok new-state)
                        (enq-token1 another-tok)
                        (set state new-state))
       tokenator    (afn ()
                        (if token-queue
                            (let q rev.token-queue
                                 (set token-queue (rev cdr.q))
                                 (tokenise car.q))
                            (aif (nextc)
                                 (do (if (is it #\newline) 
                                         (set lines (+ 1 lines))) 
                                     state.it 
                                     (self))
                                 token
                                 (do (enq-token) (self)))))
       add-to-token (fn (ch)
                        (if (no token) (set token-start char-count))
                        (fpush ch token))

       default       (fn (ch)
           (if whitespace?.ch  (add-to-token ch)
               syntax-chars.ch (enq-token1 syntax-chars.ch)
               (is ch #\.)     (enq-token1 'dot)
               (is ch #\")     (enq/switch 'left-string-delimiter in-string)
               (is ch #\#)     ((enq/switch0 in-character) ch)
               (is ch #\,)     (enq/switch0 in-unquote)
               (is ch #\;)     ((enq/switch0 in-comment) ch)
                               ((enq/switch0 in-atom) ch)))
       in-string     (fn (ch)
           (if (is ch #\\)     (do (add-to-token ch)
                                   (set state escaping))
               (is ch #\#)     (set state interpolating)
               (is ch #\")     (do (enq-token1 'right-string-delimiter 'string-fragment)
                                   (set state default))
                               (add-to-token ch)))
       interpolating (fn (ch)
           (if (is ch #\()     (do (set state default)
                                   (enq-token 'string-fragment)
                                   (add-to-token #\#)
                                   (add-to-token #\()
                                   (enq-token 'interpolation-start))
                               (do (add-to-token #\#)
                                   (if (is len.token 1) (-- token-start))
                                   ((set state in-string) ch))))
       escaping      (fn (ch)
           (add-to-token ch)
           (set state in-string))
       in-character  (fn (ch)
           (if (and (> (len token) 2) (char-terminator ch))
               ((enq/switch0 default) ch)
               (add-to-token ch)))
       in-comment    (fn (ch)
           (if (is ch #\newline) ((enq/switch0 default) ch)
                                 (add-to-token ch)))
       in-atom       (fn (ch)
           (if (or whitespace?.ch 
                   syntax-chars.ch) ((enq/switch0 default) ch)
                                    (add-to-token ch)))
       in-unquote    (fn (ch)
           (if (is ch #\@)     (enq/switch 'unquote-splicing default)
                               ((enq/switch 'unquote default) ch))))

     (set state default)

     (fn ((o option nil))
       (if (is option 'linecount)
           lines
           (is option 'in-string)
           (set state in-string)
           (tokenator)))))

(def read-tokens (text)
  "writes token information from text to stdout"
  (let tt (arc-tokeniser (instring text))
    (while (prn (tt)))))

(def parse (text)
  "parses the given text (input or string)
   and returns the corresponding arc object"
  (let (unescape-fragment assemble-string next-form read-string read-form read-list ignore) nil
    (= ignore (uniq))

    (def unescape-fragment (fragment)
      (string:rev (accum s
        ((afn (chs escaping)
          (iflet ch (car chs)
            (if escaping
                (do
                  (case ch
                    #\# (s ch)
                    #\\ (s ch)
                    #\" (s ch)
                    #\n (s #\newline)
                    #\r (s #\return)
                    #\t (s #\tab))
                  (self (cdr chs) nil))
                (case ch
                  #\\ (self (cdr chs) t)
                      (do (s ch)
                          (self (cdr chs) nil)))))) (coerce fragment 'cons) nil))))

    (def assemble-string (fragments)
      (if no.fragments
          ""
          (is len.fragments 1)
          car.fragments
          `(string ,@rev.fragments)))

    (def next-form (token token-generator)
      (if (token? token 'syntax 'left-paren)
          (read-list token-generator 'right-paren)
          (token? token 'syntax 'left-bracket)
          `(fn (_) ,(read-list token-generator 'right-bracket))
          (token? token 'syntax 'left-string-delimiter)
          (read-string token-generator nil)
          (and (token? token 'syntax) 
	             (in cadr.token 'quasiquote 'quote 'unquote 'unquote-splicing))
          `(,cadr.token ,(read-form token-generator))
          (or (token? token 'whitespace)
              (token? token 'comment))
          ignore
          cadr.token))

    (def read-string (token-generator fragments)
      (let token (token-generator)
        (if (token? token 'syntax 'right-string-delimiter)
            (assemble-string fragments)
            (token? token 'interpolation-start)
            (do (push (read-form token-generator) fragments)
                (let token2 (token-generator)
                  (if (token? token2 'syntax 'right-paren)
                      token-generator!in-string
                      (err "unclosed string interpolation: " car.fragments)))
                (read-string token-generator fragments))
            (token? token 'string-fragment)
            (do (push (unescape-fragment (cadr token)) fragments)
                (read-string token-generator fragments))
            token
            (err (string "unexpected token in string: " token ": fragments are " fragments)))))

    (def read-form (token-generator)
      (let nextform (next-form (token-generator) token-generator)
        (if (is nextform ignore)
            (read-form token-generator)
            nextform)))

    (def read-list (token-generator terminator)
      (let token (token-generator)
        (if token
            (if (token? token 'syntax 'dot)
                (car:read-list token-generator terminator)
                (~token? token 'syntax terminator)
                (let nextform (next-form token token-generator)
                  (if (is nextform ignore)
                      (read-list token-generator terminator)
                      (cons nextform (read-list token-generator terminator))))))))

    (read-form (arc-tokeniser (if (isa text 'string) (instring text) text)))))

(def token? ((kind tok s e) expected-kind (o expected-tok))
  (and (is kind expected-kind) 
       (or (no expected-tok) 
           (is tok expected-tok))))

(set syntax-pairs (obj
  left-paren            'right-paren
  left-bracket          'right-bracket
  left-string-delimiter 'right-string-delimiter))

(= (syntax-pairs (string #\# #\()) 'right-paren)

(def unmatchify (token-name)
  (sym (string "unmatched-" token-name)))

(def link-parens (right left)
  (if left
      (do (if (no:is (right 1) (syntax-pairs (left 1)))
              (do (scar cdr.left  (unmatchify:cadr left))
                  (scar cdr.right (unmatchify:cadr right))))
          (= (right 2) (left 2))
          (= (left 3) (right 3)))
      (scar cdr.right (unmatchify:cadr right))))

(def index-source (text)
  "returns a list of (tok start finish) where start and finish
   represent where the token occurs in the text, except for
   parens and brackets, where start and end are used for
   matching. Used by welder for colourising"
  (let (result parens brackets quotes tkz) nil
    
    (= tkz (arc-tokeniser (instring text)))

    (whilet token (tkz)
      (fpush token result)

      (let (kind tok start length) token
        (if (is kind 'syntax)
            (if (is tok 'left-paren)             (fpush token parens)
                (is tok 'left-bracket)           (fpush token brackets)
                (is tok 'left-string-delimiter)  (fpush token quotes)
                (is tok 'right-bracket)          (link-parens token (fpop brackets))
                (is tok 'right-string-delimiter) (link-parens token (fpop quotes))
                (is tok 'right-paren)            (let left-tok (fpop parens)
                                                    (if (is car.left-tok 'interpolation-start)
                                                        tkz!in-string)
                                                    (link-parens token left-tok)))
            (is kind 'interpolation-start)       (fpush token parens)
            (is kind 'whitespace)                (fpop result)))) 

    (each p quotes (scar cdr.p (unmatchify:cadr p)))
    (each p parens (scar cdr.p (unmatchify:cadr p)))
    (each p brackets (scar cdr.p (unmatchify:cadr p)))
    (list (rev result) (tkz 'linecount))))

(def si-repl ()
  (prn "enjoy interpolating. type x! to return to the usual repl")
  ((afn ()
        (pr "arc$ ")
        (on-err (fn (ex)
                    (prn "Error: " (details ex))
                    (self))
                (fn ()
                    (let expr (parse (stdin))
                      (if (no:is expr 'x!)
                          (do (write (eval expr))
                              (prn)
                              (self)))))))))

