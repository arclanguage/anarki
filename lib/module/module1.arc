; Born on 02/21/2008
; A minimal module system with visibility control.
; Modules can be referred to from within as `here'.
; There is no support for local macros. Perhaps use
; the new `macrolet'?
;
; (require "lib/module/module1.arc")
; (module foo (a bar) (baz)
;   (= a 42)
;   (def baz (x) (+ x 2))
;   (def bar (x) (+ x 1))
;   (prn (baz a)))
;
; (prn foo!a)
; (foo!bar 0)
; (= bar foo!bar) ; an import.
; (= a2 foo!a)    ; a qualified import.

(mac module (name public private . body)
  `(do (= ,name (table))
       (with (here ,name ,@(join (intersperse nil (join public private)) '(nil)))
         ; replace 'def with '= so that 'def won't erroneously
         ; assign help*, sig, and source-file* entries
	 ,@(map
             [if (caris _ 'def)
                ; would have used let (defsym name parms . body) _ but
                ; macros currently have issues with dotted lists
                (with ( name (cadr _) parms (car:cdr:cdr _) body (cdr:cdr:cdr _))
                   `(= ,name (fn ,parms ,@body)))
                _]
           body)
	 (= (,name 'public) ',public)
	 ,@(map (fn (a) `(= (,name ',a) ,a)) public))
       ,name))

