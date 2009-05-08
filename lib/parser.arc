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

(def arc-tokeniser (char-stream)
  (withs (make-token     (fn (kind tok start length)
                             (list kind tok start (+ start length)))
          make-character (fn (token)
                             (if (is (len token) 3)
                                 (token 2)
                                 character-constants.token
                                 character-constants.token
                                 (on-err (fn (ex) (err (string "unknown char: " token)))
                                         (fn ()   (coerce (coerce (cut token 2) 'int 8) 'char)))))
          tokenise       (fn ((token start kind))
                             (if (isa token 'sym)
                                 (make-token 'syntax token start 1)
                                 (do (= token (string:rev token))
                                     (if (all whitespace? token)
                                         (make-token 'whitespace token start len.token)
                                         kind
                                         (make-token kind token start len.token)
                                         (headmatch "#\\" token)
                                         (make-token 'char (make-character token) start len.token)
                                         (headmatch ";" token)
                                         (make-token 'comment token start len.token)
                                         (on-err (fn (ex)
                                                     (make-token 'sym (sym token) start len.token))
                                                 (fn ()
                                                     (make-token 'int (coerce token 'int) start len.token)))))))
          char-terminator (fn (ch)
                              (or syntax-chars.ch whitespace?.ch (in ch #\" #\, #\#)))
          char-count     0
          nextc          (fn ()
                             (++ char-count)
                             (readc char-stream))
          token-start    0
          token          nil
          states         nil
          state          nil
          switch-state   (fn (new-state (o ch))
                             (= state states.new-state)
                             (if ch state.ch))
          token-queue    nil
          enq-token      (fn ((o another nil) (o token-kind nil))
                             (doif token
                                   (push (list token (- token-start 1) token-kind) token-queue)
                                   (wipe token))
                             (aif another
                                  (push (list it (- char-count 1) nil) token-queue)))
          enq/switch     (fn (another-tok new-state (o ch))
                             (enq-token another-tok)
                             (switch-state new-state ch))
          tokenator      (afn () (if token-queue
                                    (let q rev.token-queue
                                         (= token-queue (rev cdr.q))
                                         (tokenise car.q))
                                    (aif (nextc)
                                         (do (state it) (self))
                                         token
                                         (do (enq-token) (self)))))
          add-to-token   (fn (ch)
                             (if (no token) (= token-start char-count))
                             (push ch token)))

    (= states (obj
      default          (fn (ch)
                           (if whitespace?.ch
                               (add-to-token ch)
                               syntax-chars.ch
                               (enq-token syntax-chars.ch)
                               (is ch #\")
                               (enq/switch 'left-string-delimiter 'in-string)
                               (is ch #\#)
                               (enq/switch nil 'in-character ch)
                               (is ch #\,)
                               (enq/switch nil 'in-unquote)
                               (is ch #\;)
                               (enq/switch nil 'in-comment ch)
                               (enq/switch nil 'in-atom ch)))
      in-string        (fn (ch)
                           (if (is ch #\\)
                               (do (add-to-token ch)
                                   (switch-state 'escaping))
                               (is ch #\#)
                               (switch-state 'interpolating)
                               (is ch #\")
                               (do (enq-token 'right-string-delimiter 'string-fragment)
                                   (switch-state 'default))
                               (add-to-token ch)))
      interpolating    (fn (ch)
                           (if (is ch #\()
                               (do (switch-state 'default)
                                   (enq-token nil 'string-fragment)
                                   (add-to-token #\#)
                                   (add-to-token #\()
                                   (enq-token nil 'interpolation-start))
                               (do (add-to-token #\#)
                                   (if (is len.token 1)
                                       (-- token-start))
                                   (switch-state 'in-string ch))))
      escaping         (fn (ch)
                           (add-to-token ch)
                           (switch-state 'in-string))
      in-character     (fn (ch)
                           (if (and (> (len token) 2)
                                    (char-terminator ch))
                               (enq/switch nil 'default ch)
                               (add-to-token ch)))
      in-comment       (fn (ch)
                           (if (is ch #\newline)
                               (enq/switch nil 'default ch)
                               (add-to-token ch)))
      in-atom          (fn (ch)
                           (if (or whitespace?.ch syntax-chars.ch)
                               (enq/switch nil 'default ch)
                               (add-to-token ch)))
      in-unquote       (fn (ch)
                           (if (is ch #\@)
                               (enq/switch 'unquote-splicing 'default)
                               (enq/switch 'unquote 'default ch)))))

    (switch-state 'default)

    (fn ((o newstate nil))
      (if newstate
          (switch-state newstate)
          (tokenator)))))

(def read-tokens (text)
  "writes token information from text to stdout"
  (let tt (arc-tokeniser (instring text))
    (while (prn (tt)))))

(def parse (text)
  "parses the given text (input or string)
   and returns the corresponding arc object"
  (let (unescape-fragment assemble-string next-form read-string read-form read-list) nil

    (def unescape-fragment (fragment)
      (string:rev (accum s
        ((afn (chs escaping)
          (iflet ch (car chs)
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
                      (do (s ch)
                          (self (cdr chs) nil)))))
        ) (coerce fragment 'cons) nil))))

    (def assemble-string (fragments)
      (if no.fragments
          ""
          (is len.fragments 1)
          car.fragments
          `(string ,@rev.fragments)))

    (def next-form ((kind tok start end) token-generator)
      (if (is tok 'left-paren)
          (read-list token-generator 'right-paren)
          (is tok 'left-bracket)
          `(fn (_) ,(read-list token-generator 'right-bracket))
          (is tok 'left-string-delimiter)
          (read-string token-generator nil)
          (in tok 'quasiquote 'quote 'unquote 'unquote-splicing)
          `(,tok ,(read-form token-generator))
          (is kind 'whitespace)
          nil
          tok))

    (def read-string (token-generator fragments)
      (let (kind tok start end) (token-generator)
        (if (is tok 'right-string-delimiter)
            (assemble-string fragments)
            (is kind 'interpolation-start)
            (do (push (read-form token-generator) fragments)
                (let (k tok s e) (token-generator)
                  (if (is tok 'right-paren)
                      token-generator!in-string
                      (err "unclosed string interpolation: " car.fragments)))
                (read-string token-generator fragments))
            (is kind 'string-fragment)
            (do (push unescape-fragment.tok fragments)
                (read-string token-generator fragments))
            tok
            (err (string "unexpected token in string: \"" tok "\"")))))

    (def read-form (token-generator)
      (next-form (token-generator) token-generator))

    (def read-list (token-generator terminator)
      (let toklist nil
           ((afn ((kind tok start end))
                 (if (no:is tok terminator)
                     (do (aif (next-form (list kind tok start end) token-generator)
                              (push it toklist))
                         (self (token-generator))))) (token-generator))
           (rev toklist)))

    (read-form (arc-tokeniser (if (isa text 'string) (instring text) text)))))

(def token? ((kind tok s e) expected-kind (o expected-tok))
  (and (is kind expected-kind) (or (no expected-tok) (is tok expected-tok))))

(def index-source (text)
  "returns a list of (tok start finish) where start and finish
   represent where the token occurs in the text, except for
   parens and brackets, where start and end are used for
   matching. Used by welder for colourising"
  (let (result parens brackets string-delimiters link-parens tkz) nil
    (= link-parens (fn (right left)
      (if left
        (do
          (= (right 2) (left 2))
          (= (left 3) (right 3)))
        (scar cdr.right (sym (string "unmatched-" (cadr right)))))))

    (= tkz (arc-tokeniser (instring text)))

    (whilet token (tkz)
      (push token result)
      (if (or (token? token 'syntax 'left-paren) (token? token 'interpolation-start))
          (push token parens)
          (token? token 'syntax 'left-string-delimiter)
          (push token string-delimiters)
          (token? token 'syntax 'left-bracket)
          (push token brackets)
          (token? token 'whitespace)
          (pop result)
          (token? token 'syntax 'right-string-delimiter)
          (link-parens token (pop string-delimiters))
          (token? token 'syntax 'right-bracket)
          (link-parens token (pop brackets))
          (token? token 'syntax 'right-paren)
          (let left-tok (pop parens)
            (if (token? left-tok 'interpolation-start)
                tkz!in-string)
            (link-parens token left-tok))))

    (each p parens   (scar cdr.p 'unmatched-left-paren))
    (each p brackets (scar cdr.p 'unmatched-left-bracket))
    (rev result)))

