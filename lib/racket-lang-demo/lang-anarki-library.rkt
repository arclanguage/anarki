#lang anarki
(:provide anarki-library-export)

(= racket-deep-dependency-export
  ($:dynamic-require "racket-deep-dependency.rkt"
    'racket-deep-dependency-export))
($:dynamic-require "lang-anarki-deep-dependency.rkt" #f)
(load "plain-anarki-deep-dependency.arc")

(= anarki-library-export
  (+ "from " racket-deep-dependency-export ", "
     lang-anarki-deep-dependency-export ", and "
     plain-anarki-deep-dependency-export "!"))
