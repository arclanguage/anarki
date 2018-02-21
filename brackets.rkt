#lang racket/base

; From Eli Barzilay, eli@barzilay.org

;> (require "brackets.rkt")
;> (current-readtable bracket-readtable)
;> '([+ _ 1] 10)
;'((make-br-fn (+ _ 1)) 10)


(provide
  bracket-readtable
  (rename-out [*read read] [*read-syntax read-syntax]))


; main reader function for []s
; recursive read starts with default readtable's [ parser,
; but nested reads still use the curent readtable:

(define (read-square-brackets ch port src line col pos)
  `(make-br-fn
     ,(read/recursive port #\[ #f)))

; a readtable that is just like the builtin except for []s

(define bracket-readtable
  (make-readtable #f #\[ 'terminating-macro read-square-brackets))

; these two implement the required functionality for #reader

;(define (*read inp)
;  (parameterize ([current-readtable bracket-readtable])
;    (read inp)))

(define (*read . args)
  (parameterize ([current-readtable bracket-readtable])
    (read (if (null? args) (current-input-port) (car args)))))

(define (*read-syntax src port)
  (parameterize ([current-readtable bracket-readtable])
    (read-syntax src port)))

; and the need to be provided as `read' and `read-syntax'
