#lang info
(define collection "anarki")
(define version "0.0")
(define pkg-desc "Community-managed variant of the Arc dialect of Lisp")
(define license 'Artistic-2.0)

(define deps '("base" "sha"))
(define build-deps '("racket-doc" "scribble-lib"))

(define scribblings '(("scribblings/anarki.scrbl" ())))
