#lang racket/base

(require racket/runtime-path)

(define-runtime-path boot-scm-path "boot.scm")

(define tl (dynamic-require boot-scm-path 'tl))

(provide tl)
