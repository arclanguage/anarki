;defpat.arc
;by AmkG
; define using pattern matching
;
;ex
;  (p-m:def map1-pat
;     (f ())
;       ()
;     (f (x . xs))
;       (cons (f x) (map1 f xs)))
;abstract syntax form:
; (defpat <name>
;     [<docstring>]
;     [<pattern> <clause>]*
;     [<clause>]
; )
;<docstring> := <string>
;<pattern> :=
;  <const> | <var> | <guarded> | (<pattern>*) | (<pattern>* . <pattern>)
;<const> := anything that is: (not a symbol), or (t and nil), or a 'symbol form.
;<var> := anything that is a symbol, but isn't t and isn't nil
;<guarded> :=
;   ,(<var> <test(var)>) | ,@(<test(_)>)
;<test(var)> := an expression involving var; this pattern matches if 
;  this expression returns true for that var
;<clause> := any expression
;
; - like any Arc conditional form, if the last clause doesn't have a
;   pattern, it's the "default" case.
; - You probably want to use (<pattern>*) for the pattern attached to
;   a clause.  If it's a <var>, then it will automatically match anything
;   and everything passed to this function.  If it's a <const>, it will
;   never match, because what is actually passed to a function is a list
;   of its arguments.  Of course, you can use <const> if it's inside
;   a list pattern.
; - () is also nil, so you can match directly to empty lists.
; - list matching is exact; if you put 3 elements, it will match
;   lists with *exactly* 3 elements, never 2 or 4.
; - '<symbol> is defined as a <const>.  This means you can match to
;   a symbol in the argument by using '<symbol>, since <symbol> will
;   match to a variable.  This also means that you should avoid using
;   'quote as a <var>.
; - strings aren't symbols, so they are also defined as <const>
; - also see the tests at the end of this sourcefile.
;
;SUBTLETIES (not bugs!):
; - It's possible to match constants after a dot in a dotted list.
; - If all patterns are lists of the same length, the function
;   will have an exact number of arguments; if all patterns are
;   lists of at least N length, the function will have at least N
;   arguments.
; - (x) <clause> <clause> is slightly different from
;   (x) <clause> dontcare <clause> - the first form will create a
;   function that accepts exactly one argument, the second form will
;   create a function that accepts any number of arguments.
; - (x) <clause1> (y) <clause2> will bind x for <clause1>, and y for
;   <clause2>; however, x will be free in <clause2>, and y will be
;   free in <clause1>.
;
;TODO:
; - 'defpat does not rearrange patterns, so you must manually arrange
;   from most specific to most generic patterns for now.  Obviously it's
;   possible to do this programatically but I will defer it for now.
; - code cleanup
;
;
;please report any bugs you don't want to fix yourself to:
; almkglor@gmail.com
;alternatively, throw it on http://arclanguage.org/forum

(mac defpat (name . body)
  " Defines a function named `name' using pattern-matching.
    See also [[p-m]] "
  (ero "Warning: 'defpat is deprecated.  Use 'p-m:def instead.")
  `(p-m:def ,name ,@body))

(mac p-m ((macro . body))
  " Modifies a function definition form to use patterns-matching.
    (p-m:afn
    ((x . xs))  (cons (f x) (self xs))
    (())       ())
    See also [[defpat]] "
  ; special handling for some macros because
  ; their signatures are not in the sig* table
  (if (in macro 'def 'mac)
      `(,macro ,(car body) ,@(*defpat-internal (cdr body)))
      (is macro 'fn)
      `(fn ,@(*defpat-internal body))
      ; else
      (let s (sig* macro)
        (unless s
          (err (string "p-m: unknown macro " macro)))
        (unless (dotted s)
          (err (string "p-m: macro " macro
                       " does not accept body")))
        ; count the number of places to skip
        (withs
         (counter
          (fn (l)
            ((afn (l n)
               (if (acons l)
                   (self (cdr l) (+ n 1))
                   n))
             l 0))
          ; assuming parms is just before the body
          skips     (- (counter s) 1)
          preparms  (cut body 0 skips)
          pat-pairs (nthcdr skips body))
         `(,macro ,@preparms ,@(*defpat-internal pat-pairs))))))

(def *defpat-internal (origbody)
  " Composes the form for the `defpat' macro; used for debug. "
  (withs
   (
    ; in case pg decides to change ,x and ,@x some time
    ; in the future
    unquote-sym    (car ',x)
    unquote-at-sym (car ',@x)
    docstring      (if (isa (car origbody) 'string) (car origbody))
    body           (if docstring (cdr origbody) origbody)
    ; a clause is a pattern-expression pair
    clauses (pair body)
    ; proper clauses are pattern-expression pairs,
    ; for else clauses there are no patterns
    properclauses (keep [is 2 (len _)] clauses)
    ; extract the pattern
    patn (fn (x) (if (is 2 (len x)) (car x)))
    ; extract the expression
    exp (fn (x) (if (is 2 (len x)) (cadr x)))
    ; check if variable name
    varp [and (isnt _ nil) (isnt _ t) (isa _ 'sym)]
    ; check if 'x form
    quoteformp [caris _ 'quote]
    ; check if constant
    constp (orf (andf atom ~varp) quoteformp)
    ; check if ,(x (test x)) form
    guardedp [caris _ unquote-sym]
    ; check if ,@(test _) form
    guarded-atp [caris _ unquote-at-sym]
    ; accessors of ,(x (test x))
    var-guarded  car:cadr
    test-guarded cadr:cadr
    ; accessors of ,@(test _)
    test-guarded-at cadr
    ; cuts out dotted portions of dotted lists
    cutdotted (fn (l)
                (if (dotted l)
                    ((afn (acc l)
                       (if (~acons l)
                           (rev acc)
                           (self (cons (car l) acc)
                                 (cdr l))))
                     nil l)
                    l))
    ; gets the length, if it is in fact a list
    lenif (fn (l) (if (acons l) (len l) 0))
    ; isrest if each pattern has different lengths, or if
    ; there is some dotted or symbol pattern in clauses
    isrest (or (apply ~is (map lenif:cutdotted:patn properclauses))
               (some (orf dotted:patn varp:patn)
                     properclauses))
    numarg (best < (map lenif:cutdotted:patn properclauses))
    arglist ((afn (n)
               (if (is 0 n)
                   (if isrest (uniq) ())
                   (cons (uniq) (self (- n 1))) ))
             numarg)
    ; recomposes the arguments into a single list.
    recompose (fn ()
                ((afn (l)
                   (if (~acons l)
                       l
                       `(cons
                         ,(car l)
                         ,(self (cdr l)))))
                 arglist))
    ; extracts the pattern as a with-list
    patwith
      (fn (a p)
        ; note that this method of recursion will
        ; capture any nil's terminating a proper
        ; list; this is deliberate, and forms the
        ; basis of our exact-list-length checking
        (accum acc
          ((afn (p path)
             (if (constp p) nil
                 (varp p) (acc `(,p ((compose ,@path) ,a)))
                 (guardedp p) (acc `(,(var-guarded p) ((compose ,@path) ,a)))
                 (guarded-atp p) nil
                 (do
                   (self (car p) (cons 'car path))
                   (self (cdr p) (cons 'cdr path)))))
           p nil)))
    ; extracts the list of checks for a pattern
    patcheck
      (afn (a p)
        (if (constp p) `(is ,a ,p)
            (varp p) `t
            (guardedp p) `(let ,(var-guarded p) ,a
                               ,(test-guarded p))
            (guarded-atp p) `(let _ ,a
                               ,(test-guarded-at p))
            `(and
              (acons ,a)
              (let ,a (car ,a) ,(self a (car p)))
              (let ,a (cdr ,a) ,(self a (cdr p))))))
    ; creates a clause checking form in 'if format
    clausecheck
      (fn (a c)
        ; for default cases (no pattern), the expression
        ; directly
        (if (is 1 (len c)) c
            (list
             ([patcheck a (patn _)] c)
             `(with ,(apply join ([patwith a (patn _)] c))
                ,(exp c)))))
    ; creates the dispatch form
    dispatch
      (fn ()
        ; first, recompose the arguments into a single
        ; list to make it easier to decompose (the only
        ; reason we built the argument list in the
        ; first place was so that arc would properly
        ; handle the arity).
        (w/uniq a
          `(let ,a ,(recompose)
             ; now, nestedly check each
             ; pattern
             (if ,@(mappend
                    [clausecheck
                     a _]
                    clauses))))))
   `(,arglist
     ,@(if docstring (list docstring))
     ,(dispatch))))

; test cases

(p-m:def *defpat-test1
  " The first test for defpat, testing arity-checks.
    (*defpat-test1 1) should reach the last clause, and
    must be different from (*defpat-test1 1 nil) "
  (1 2)   (prn "You chose the (1 2) form!")
  (1 x)   (prn "You didn't choose the (1 2) form, you chose the (1 " x ") !")
  (1 2 x) (prn "You added an " x " to a (1 2) form!")
  (x)     (prn "You only gave one parameter, " x "!"))

(p-m:def *defpat-test2
  " The second test for defpat, testing fixed-arity function. "
  (2 3) (prn "You chose the (2 3) form!")
  (2 x) (prn "You could have chosen the (2 3) form, but you chose"
             " (2 x) instead, with x = " x)
  (x y) (prn "Hey!  You could have chosen a (2 3) or (2 x) form,"
             " but you got (x y), with x = " x ", y = " y)
  #||#  (prn "what the... you couldn't possibly have gotten here!"))

(p-m:def *defpat-test3
  " The third test for defpat, testing list destructuring. "
  (4 5)
    (prn "You chose the (4 5) form!")
  ((4 5) x)
    (prn "You chose the (4 5) form... oops, you put it in too"
         " much!  You got the ((4 5) x) with x = " x " !!")
  ((x y) z)
    (prn "You chose the... okay, you not only put in too much,"
         " you got the (4 5) wrong... you gave me ((" x " "
         y ") " z ")")
  (z 5)
    (prn "You chose the (z 5) form, with z = " z " .... Maybe"
         " I can interest you in the (4 5) form instead?")
  x
    (prn "What?  I don't understand your parameters " x " !!"))

(p-m:def *defpat-test4
  " The fourth test for defpat, testing quoted symbols "
  ('start)
    (prn "Vroom, vroom... started!")
  ('go mph)
    (prn "Going at " mph " miles per hour!!")
  ('on "light")
    (prn "Turned on the lights!  Bright street!")
  ('on "radio")
    (prn "Yeah, I'm jamming to them music, yeah!")
  ('stop)
    (prn "Eeeeeeek!  Stopped!")
  ; otherwise
    (prn "Hey, your car can't understand that!"))

(p-m:def *defpat-pair
  " Example/testcase for defpat in redefining
    the `pair' function. "
  ((x y . zs))   `((,x ,y) ,@(*defpat-pair zs))
  ((x y . zs) f) `(,(f x y) ,@(*defpat-pair zs f))
  ((x) . _)      `((,x)) ; this is how the arc0 pair does it
  (() . _)       ())

(p-m:def *defpat-rpn
  " Example/testcase for defpat in using
    ,(x (test x)) guarded patterns "
  ; start with an empty stack
  (lst) (*defpat-rpn lst '())
  ; all done; return what's on the stack
  (() (x)) x
  ; extra stuff on the stack
  (() (_ . __)) (ero "extra stuff on the stack")
  ; divide by zero
  ((,@(is _ /) . _) (0 x . __)) (ero "divide by zero")
  ; pop, pop, push...
  ((,(f (isa f 'fn)) . xs) (b a . s)) (*defpat-rpn xs `(,(f a b) ,@s) )
  ; not enough to pop
  ((,@(isa _ 'fn) . xs) x)  (ero "too few parameters")
  ;just push
  ((x . xs) s) (*defpat-rpn xs `(,x ,@s)))

(def *defpat-afn-test (x)
  ((p-m:afn
    ((x . xs)) (cons (+ 1 x) (self xs))
    (())       nil)
   x))

