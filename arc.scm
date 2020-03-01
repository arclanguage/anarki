#lang racket/load

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)
(require racket/base)

(load "ac.rkt")
(require 'ac)

(require "brackets.rkt")
;(use-bracket-readtable)

(anarki-init-in-main-namespace-verbose)
; (aload "arc.arc")
(aload "libs.arc") 


