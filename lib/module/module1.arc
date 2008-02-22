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
	 ,@body
	 (= (,name 'public) ',public)
	 ,@(map (fn (a) `(= (,name ',a) ,a)) public))
       ,name))

