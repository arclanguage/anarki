; From Eli Barzilay, eli@barzilay.org

;> (require "brackets.scm") 
;> (use-bracket-readtable) 
;> ([+ _ 1] 10) 
;11

(module brackets mzscheme
  
; main reader function for []s
; recursive read starts with default readtable's [ parser,
; but nested reads still use the curent readtable:

(define (read-square-brackets ch port src line col pos)
  `(fn (_)
     ,(read/recursive port #\[ #f)))
  
; a readtable that is just like the builtin except for []s

(define bracket-readtable
  (make-readtable #f #\[ 'terminating-macro read-square-brackets))
  
; call this to set the global readtable

(provide use-bracket-readtable)

(define (use-bracket-readtable)
  (current-readtable bracket-readtable))
  
; these two implement the required functionality for #reader
    
;(define (*read inp)
;  (parameterize ((current-readtable bracket-readtable))
;    (read inp)))

(define (*read . args)
  (parameterize ((current-readtable bracket-readtable))
    (read (if (null? args) (current-input-port) (car args)))))

(define (*read-syntax src port)
  (parameterize ((current-readtable bracket-readtable))
    (read-syntax src port)))

; and the need to be provided as `read' and `read-syntax'

(provide (rename *read read) (rename *read-syntax read-syntax))

)
