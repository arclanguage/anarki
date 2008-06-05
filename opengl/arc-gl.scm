; This file provides OpenGL support for Arc
; Copyright 2008 Ken Shirriff
; http://arcfn.com

(load "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)

(aload "arc.arc")
(aload "libs.arc")

; Create a non-blocking REPL based on a text field

(define frame (instantiate frame% ("Arc REPL")))
(send frame show #t)

(define (cb-submit a b) (on-err (lambda (c) 
                                  (append-tf (exn-message c)))
                                (lambda ()
                                  (append-tf (send in-field get-value))
                                  (append-tf (format "~a" (ac-denil (arc-eval (read (open-input-string (send in-field get-value)))))))
                                  (send in-field set-value ""))))


(define tf (instantiate text-field% ("" frame) (style '(multiple)) (min-width 600) (min-height 150) (enabled #f)))

(define (append-tf str) (send tf set-value (string-append (send tf get-value) str "\n")))

(define in-field (instantiate text-field% ("" frame) (style '(single)) ))

(define bb  (instantiate button% ("submit" frame) (style '(border)) (callback cb-submit)))



; Provide hooks into OpenGL

(require (lib "mred.ss" "mred")
         (lib "class.ss")
         (lib "math.ss")
         (prefix gl- (lib "sgl.ss" "sgl"))
         (lib "gl-vectors.ss" "sgl"))

; List of functions to export from Scheme to Arc
(map (lambda (s) (xdef s (eval s)))
     '(gl-shade-model gl-normal gl-begin gl-end gl-vertex gl-clear-color gl-clear gl-push-matrix gl-pop-matrix gl-rotate gl-translate gl-call-list gl-flush gl-light-v gl-enable gl-new-list gl-gen-lists gl-material-v gl-viewport gl-matrix-mode gl-load-identity gl-frustum gl-light-v gl-enable gl-end-list gl-scale gl-color gl-line-width gl-point-size gl-disable sin cos))

; Arc doesn't provide access to vector, so make gl-float-vector take individual arguments
; instead of a vector
(xdef 'gl-float-vector (lambda (a b c d) (vector->gl-float-vector (vector a b c d))))

; eval-scheme allows Scheme code to be executed from Arc.
; Need to convert Arc nils to Scheme and back.
; Also Arc messes up #f, so define 'false as a synonym for #f
(define (my-ac-denil x)
  (cond ((pair? x) (cons (my-ac-denil-cdr (car x)) (my-ac-denil-cdr (cdr x))))
        (#t x)))

(define (my-ac-denil-cdr x)
  (cond ((eq? x 'nil) '())
        ((eq? x 'false) #f)
        (#t (my-ac-denil x))))

(xdef 'eval-scheme (lambda (stuff)(eval (my-ac-denil stuff))))