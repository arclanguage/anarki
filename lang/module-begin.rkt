#lang racket/base

(require
  (only-in racket/path simple-form-path)
  (only-in racket/runtime-path define-runtime-path)

  (only-in anarki
    anarki-eval anarki-init-in-main-namespace anarki-main-namespace)

  (for-syntax racket/base))

(provide (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx (:provide)

    ; For future compatibility, we require each #lang anarki file to
    ; begin with a (:provide ...) form. If we come up with other,
    ; potentially better ways to integrate Arc code with Racket, we
    ; can use keywords like :provide to distinguish between the
    ; different options.
    [(_ (:provide export ...) body ...)

     #`(#%module-begin

         ; We break hygiene just enough here so that we're resolving
         ; the path of the file where this 'module-begin syntax is
         ; called rather than where it's defined.
         #,(datum->syntax stx
             `(,#'define-runtime-path ,#'here-nonsimple ,#'"."))

         ; The resolved runtime path has an unnecessary "." at the
         ; end, so we strip it off.
         (define here (simple-form-path here-nonsimple))

         (anarki-init-in-main-namespace)

         ; Arc code often uses (load ...) to look up other files, so
         ; we set the working directory to the location of the Arc
         ; code file so that the behavior of (load ...) is more
         ; predictable. This is also the default directory Racket
         ; consults for (dynamic-require ...), so loading Racket
         ; libraries from Arc is similar to loading Arc libraries from
         ; Arc.
         (define (eval-here expr)
           (parameterize ([current-namespace anarki-main-namespace]
                          [current-directory here])
             (anarki-eval expr)))

         (let ()
           ; We evaluate each top-level exprssion in the file.
           (eval-here 'body)
           ...

           ; We avoid printing the result of the last expression to
           ; the output port.
           (void))

         ; We export each of the specified exports. It's almost silly
         ; to export these things, since there's nothing stopping a
         ; client from running (anarki-eval ...) for whatever global
         ; variable they want to access, but it makes it makes Arc
         ; libraries blend in better with other Racket libraries.
         (begin
           (define export-value (eval-here 'export))
           (provide (rename-out [export-value export])))
         ...)]))
