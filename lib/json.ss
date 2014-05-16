;; Scheme JSON parser from http://planet.plt-scheme.org/display.ss?package=json.plt&owner=dherman
;; Substantial changes to support arc data structures and json spec.
;; For any future changes please refer to spec found at http://json.org/

;; To Use:
;; ($ (require (file "lib/json.ss")))
;; ($ (xdef read-json read-json))
;; ($ (xdef write-json write-json))

;; Test Cases:
;; (tostring (write-json '(1 2 3)))
;; (tostring (write-json "server"))
;; (tostring (write-json 3))
;; (tostring (write-json 3.1476777))
;; (tostring (write-json 'null))
;; (tostring (write-json #t))
;; (tostring (write-json #f))
;; (tostring (write-json 'true))
;; (tostring (write-json 'false))
;; (tostring (write-json 'symtostr))
;; (tostring (write-json (obj "server" "cool server" "success" 'true)))
;; (fromstring x (read-json (stdin))) ;; where x is any of the above

#lang scheme/base
(require (only-in scheme/base [read scheme:read] [write scheme:write]))
(provide read-json write-json jsexpr->json json->jsexpr jsexpr?)

(define (write-json json [port (current-output-port)])
  (cond
    [(hash? json)
     (display "{" port)
     (for ([(key value) json]
           [i (in-naturals)])
       (when (> i 0)
         (display ", " port))
       (fprintf port "\"~a\"" key)
       (display ": " port)
       (write-json value port))
     (display "}" port)]
    [(pair? json)
      (display "[" port)
	     (for ([(value i)(in-indexed (arc-list-denil json))])
         (when (> i 0)
           (display ", " port))
         (write-json value port))
     (display "]" port)]
   [(arc-boolean? json)
     (scheme:write (if (eq? json 'false) 'false 'true) port)]
   [(or (char? json)(string? json)(number? json)(integer? json)(arc-nil? json))
     (scheme:write json port)]
   [(symbol? json)
     (scheme:write (symbol->string json) port)]
    [else (error 'json "bad json value: ~v" json)]))

; arc data handlers

(define (arc-list-denil x)
  (cond ((pair? x) (cons (arc-denil-car (car x)) (arc-denil-cdr (cdr x))))
        (#t x)))

(define (arc-denil-car x)
  (if (eq? x 'nil)
      'nil
      (arc-list-denil x)))

(define (arc-denil-cdr x)
  (if (eq? x 'nil)
      '()
      (arc-list-denil x)))

(define (arc-nil? x)
  (or (eq? x 'nil)
      (eq? x 'null)))

(define (arc-boolean? x)
  (or (eq? x 'true)
      (eq? x 'false)
      (eq? x #t)
      (eq? x #f)))

; reader

(define (read-json [port (current-input-port)])
  (case (peek-char port)
    [(#\{) (read/hash port)]
    [(#\[) (read/list port)]
    [(#\") (read/string port)]
    [(#\t) (read/true port)]
    [(#\f) (read/false port)]
    [(#\n) (read/null port)]
    [else (read/number port)]))

(define (expect ch . expected)
  (unless (memq ch expected)
    (error 'read "expected: ~v, got: ~a" expected ch))
  ch)

(define (expect-string port expected)
  (list->string (for/list ([ch expected])
                  (expect (read-char port) ch))))

(define (skip-whitespace port)
  (let ([ch (peek-char port)])
    (when (char-whitespace? ch)
      (read-char port)
      (skip-whitespace port))))

(define (in-port-until port reader done?)
  (make-do-sequence (lambda ()
                      (values reader
                              (lambda (port) port)
                              port
                              (lambda (port)
                                (not (done? port)))
                              (lambda values #t)
                              (lambda (port . values) #t)))))

(define (read/hash port)
  (expect (read-char port) #\{)
  (skip-whitespace port)
  (begin0 (for/hasheq ([(key value)
                        (in-port-until port
                                       (lambda (port)
                                         (let ([key (read/string port)])
                                           (unless (string? key)
                                             (error 'read "expected: string, got: ~v" key))
                                           (skip-whitespace port)
                                           (expect (read-char port) #\:)
                                           (skip-whitespace port)
                                           (let ([value (read-json port)])
                                             (skip-whitespace port)
                                             (expect (peek-char port) #\, #\})
                                             (values (string->symbol key) value))))
                                       (lambda (port)
                                         (eq? (peek-char port) #\})))])
            (when (eq? (peek-char port) #\,)
              (read-char port))
            (skip-whitespace port)
            (values key value))
          (expect (read-char port) #\})))

(define (read/list port)
  (expect (read-char port) #\[)
  (begin0 (for/list ([value
                      (in-port-until port
                                     (lambda (port)
                                       (skip-whitespace port)
                                       (begin0 (read-json port)
                                               (skip-whitespace port)
                                               (expect (peek-char port) #\, #\])))
                                     (lambda (port)
                                       (eq? (peek-char port) #\])))])
            (when (eq? (peek-char port) #\,)
              (read-char port))
            value)
          (expect (read-char port) #\])))

(define (read/string port)
  (expect (read-char port) #\")
  (begin0 (list->string
           (for/list ([ch (in-port-until port
                                         (lambda (port)
                                           (let ([ch (read-char port)])
                                             (when (eof-object? ch)
                                               (error 'read "unexpected EOF"))
                                             (if (eq? ch #\\)
                                                 (let ([esc (read-char port)])
                                                   (when (eof-object? ch)
                                                     (error 'read "unexpected EOF"))
                                                   (case esc
                                                     [(#\b) #\backspace]
                                                     [(#\n) #\newline]
                                                     [(#\r) #\return]
                                                     [(#\f) #\page]
                                                     [(#\t) #\tab]
                                                     [(#\\) #\\]
                                                     [(#\") #\"]
                                                     [(#\/) #\/]
                                                     [(#\u) (unescape (read-string 4 port))]
                                                     [else esc]))
                                                 ch)))
                                         (lambda (port)
                                           (eq? (peek-char port) #\")))])
             ch))
          (expect (read-char port) #\")))

(define (unescape str)
  (unless (regexp-match #px"[a-fA-F0-9]{4}" str)
    (error 'read "bad unicode escape sequence: \"\\u~a\"" str))
  (integer->char (string->number str 16)))

(define (read/true port)
  (expect-string port "true")
  #t)

(define (read/false port)
  (expect-string port "false")
  #f)

(define (read/null port)
  (expect-string port "null")
  'nil)

(define (read/digits port)
  (let ([digits (for/list ([digit (in-port-until port
                                                 read-char
                                                 (lambda (port)
                                                   (let ([ch (peek-char port)])
                                                     (or (eof-object? ch)
                                                         (not (char-numeric? ch))))))])
                  digit)])
    (when (and (null? digits) (eof-object? (peek-char port)))
      (error 'read "unexpected EOF"))
    (when (null? digits)
      (error 'read "expected: digits, got: ~a" (peek-char port)))
    digits))

(define (read/number port)
  (let* ([sign (read/sign? port)]
         [digits (read/digits port)]
         [frac (read/frac? port)]
         [exp (read/exp? port)])
    (string->number
     (list->string
      (append sign digits frac exp)))))

(define (read/sign? port)
  (case (peek-char port)
    [(#\- #\+) (list (read-char port))]
    [else '()]))

(define (read/exp? port)
  (if (memq (peek-char port) '(#\e #\E))
    (begin
      (skip-peek port)
      (let ([sign (read/sign? port)])
        (append (list #\e) sign (read/digits port))))
    '()))

(define (read/frac? port)
  (if (eq? (peek-char port) #\.)
    (begin
      (skip-peek port)
      (append '(#\.) (read/digits port)))
    '()))

(define (skip-peek port)
  (read-char port))

(define (jsexpr? x)
  (or (integer? x)
      (and (number? x) (inexact? x))
      (null-jsexpr? x)
      (boolean? x)
      (string? x)
      (null? x)
      (array-jsexpr? x)
      (object-jsexpr? x)))

(define (array-jsexpr? x)
  (or (null? x)
      (and (pair? x)
           (jsexpr? (car x))
           (array-jsexpr? (cdr x)))))

(define (object-jsexpr? x)
  (let/ec return
    (and (hash? x)
         (for ([(key value) x])
           (unless (and (symbol? key) (jsexpr? value))
             (return #f)))
         #t)))

(define (null-jsexpr? x)
  (eqv? x #\null))

(define null-jsexpr #\null)

(define (jsexpr->json x)
  (let ([out (open-output-string)])
    (write-json x out)
    (get-output-string out)))

(define (json->jsexpr s)
  (let ([in (open-input-string s)])
    (read-json in)))
