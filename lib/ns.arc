; ns.arc
;
; Ross Angle (rocketnia) 2011
;
; Utilities for managing Racket modules and namespaces.
;
; This library isn't exactly a full Arc module system, but it may be
; useful or instructive to anyone who's inspired to make one. As far
; as I (rocketnia) can tell, the biggest hurdles remaining to making a
; good module system are as follows:
;
;   - Arc macros are a little bit too unhygienic. You can't just
;     import a macro and use it right away, 'cause you also have to
;     import all the variables the macro's expansions could refer to.
;
;   - An Arc module system may need to keep track of other things than
;     global variables, such as type symbols and global table entries.
;     For instance, if two Arc libraries both use types named
;     'special-fn with 'defcall behavior, you get type symbol conflict
;     *and* table entry conflict (the entry for coerce*!fn!special-fn)
;     at the same time.
;
; Parts of these issues can be solved by having all Arc programmers
; follow certain conventions in their code. However, there's a good
; chance the Arc core itself needs to follow those conventions in
; order for the module system to feel seamless, and that may change
; the language to the point that it isn't *really* Arc (whatever that
; means).
;
; This introduces seven types: 'rns, 'anchor, 'ns, 'module, 'rmodule,
; 'modecule, and 'rns-var. The R stands for "Racket," the NS stands
; "namespace", and the MODECULE stands for justice and freedom. For
; each type that starts with R, the non-R version (if there is one)
; automatically converts between Arc variable names and Racket
; variable names so that it's easy to do a certain amount of
; acrobatics with modules and namespaces without acknowledging their
; Racket origins.
;
; Modecules are a bit of an experiment that doesn't seem so useful
; after the fact. Short for "module molecule," they're like modules
; but with one export, which can be imported under various names using
; '{r,}ns-set-modecule. It would be even more useful to have
; '{r,}ns-set-import utilities which take care of renaming using an
; optional "original name" argument. TODO: Implement that.
;
; This library also requires a slight change to ac.scm: Instead of Arc
; global variables being set using 'namespace-set-variable-value!,
; they're now set using 'set!, and the Racket version of an Arc
; command is compiled using 'compile-allow-set!-undefined. See
; 'arc-exec in ac.scm for more details about how this helps.

($:require
  (only racket/base
    local-require
    make-derived-parameter
    namespace-anchor?
    namespace-anchor->empty-namespace))


(defextend type (x) $.namespace?.x
  'rns)

(defextend type (x) $.namespace-anchor?.x
  'anchor)


(def ns-racketarc (x)
  (annotate 'ns (list rnsify.x)))

(def ns-arcracket (x)
  (!0:rep nsify.x))

(def global-racketarc (global)
  " Converts a Racket top-level variable name (a symbol) into the name
    the variable is visible as from Arc. "
  (unless $.symbol-interned?.global
    (err "Can't global-racketarc a gensym."))
  (let name string.global
    (unless (begins name "_")
      (err:+ "Can't global-racketarc " (tostring write.global) "."))
    (sym:cut name 1)))

(def global-arcracket (global)
  " Converts an Arc global variable name (a symbol) into the
    corresponding Racket top-level variable name. "
  (unless $.symbol-interned?.global
    (err "Can't global-arcracket a gensym."))
  ac-global-name.global)

(def could-global-racketarc (global)
  (and (isa global 'sym)
       $.symbol-interned?.global
       (begins string.global "_")))

(def safely-global-racketarc (global)
  (when could-global-racketarc.global
    (list global-racketarc.global)))

(def safely-map-global-racketarc (globals)
  (mappend safely-global-racketarc globals))


(def rnsify (x)
  (err:+ "Can't rnsify " (tostring write.x)))

(defextend rnsify (x) (isa x 'rns)
  x)

(def nsify (x)
  (err:+ "Can't nsify " (tostring write.x)))

(defextend nsify (x) (isa x 'ns)
  x)


(defvar current-rns
  ($.make-derived-parameter $.current-namespace rnsify idfn))

(def call-w/current-rns (rns body)
  (parameterize defvar-impl.current-rns rns
    (body)))

(mac w/current-rns (rns . body)
  `(call-w/current-rns ,rns (fn () ,@body)))

(defvar current-ns ($.make-derived-parameter defvar-impl.current-rns
                     ns-arcracket ns-racketarc))

(def call-w/current-ns (ns body)
  (parameterize defvar-impl.current-ns ns
    (body)))

(mac w/current-ns (ns . body)
  `(call-w/current-ns ,ns (fn () ,@body)))


(eval `($:define-namespace-anchor ,global-arcracket!main-ns-anchor*))

(def anchor-rns ((o anchor main-ns-anchor*))
  $.namespace-anchor->namespace.anchor)

(def anchor-empty-rns ((o anchor main-ns-anchor*))
  $.namespace-anchor->empty-namespace.anchor)

(defextend rnsify (x) (isa x 'anchor)
  anchor-rns.x)

(def anchor-ns ((o anchor main-ns-anchor*))
  (ns-racketarc anchor-rns.anchor))

(def anchor-empty-ns ((o anchor main-ns-anchor*))
  (ns-racketarc anchor-empty-rns.anchor))

(defextend nsify (x) (isa x 'anchor)
  (ns-racketarc rnsify.x))


(= anon-module-prefix* (string (uniq) '-module-))

(= racket/bare-bones--plain-module-begin (uniq))
(= racket/bare-bones--define (uniq))
(= racket/bare-bones--define-customvar (uniq))
(= racket/bare-bones--quote (uniq))
(= racket/bare-bones--datum (uniq))
(= racket/bare-bones--set (uniq))
(= racket/bare-bones--app (uniq))
(= racket/bare-bones--require (uniq))
(= racket/bare-bones--provide (uniq))

; We define racket/bare-bones, a stripped-down version of racket/base
; where every variable is named as a gensym, just to make sure it's
; possible for our module utilities to define modules which provide
; exports with names like 'define or '#%app which would clash with the
; things used to define those very modules.
;
(eval `($:module racket/bare-bones racket/base
         (require (for-syntax racket/base))
         (define-syntax-rule (define-customvar var getter setter)
           (define-syntax var
             (make-set!-transformer
               (lambda (stx)
                 (syntax-case stx (set!)
                   (id (identifier? #'id) #'(getter))
                   ((set! _ val) #'(setter val)))))))
         (provide (rename-out
                    (#%plain-module-begin
                      ,racket/bare-bones--plain-module-begin)
                    (define     ,racket/bare-bones--define)
                    (define-customvar
                      ,racket/bare-bones--define-customvar)
                    (quote      ,racket/bare-bones--quote)
                    (#%datum    ,racket/bare-bones--datum)
                    (set!       ,racket/bare-bones--set)
                    (#%app      ,racket/bare-bones--app)
                    (#%require  ,racket/bare-bones--require)
                    (#%provide  ,racket/bare-bones--provide)))))

; We manipulate a Racket module as a module path together with a
; Racket namespace the module is registered in.
(def pathed-rmodule (rns path)
  (annotate 'rmodule (list rnsify.rns path)))

(def rmodulify (x)
  (err:+ "Can't rmodulify " (tostring write.x) "."))

(defextend rmodulify (x) (isa x 'rmodule)
  x)

(def modulify (x)
  (err:+ "Can't modulify " (tostring write.x) "."))

(defextend modulify (x) (isa x 'module)
  x)

(def module-racketarc (rmodule)
  (annotate 'module (list rmodulify.rmodule)))

(def module-arcracket (module)
  (!0:rep modulify.module))

(def rmodule-keys (rmodule)
  (let (rns path)  (rep rmodulify.rmodule)
    (mappend (fn ((phase . exports)) (map car exports))
      ($.call-with-values
        (fn () (w/current-rns rns $.module->exports.path)) join))))

(def module-keys (module)
  (safely-map-global-racketarc:rmodule-keys:module-arcracket
    modulify.module))


(def instantiate-rmodule (rmodule)
  " Instantiates a Racket module by first delving into its internal
    Arc as a combination of a module path and an example namespace
    which has the module attached on that path, and then requiring
    that path in that namespace. "
  (let (rns path) (rep rmodulify.rmodule)
    (w/current-rns rns ($.dynamic-require path ($.void))))
  rmodule)

(def embed-racket/bare-bones (result)
  " Makes a `racket/bare-bones' expression out of the literal value
    `result' by embedding it inside a procedure call. This is
    necessary so that Racket doesn't translate it into immutable
    syntax and back. "
  ($.list racket/bare-bones--app
    (cons racket/bare-bones--datum (fn () result))))

(def make-bare-bones-rmodule (racket-module-body)
  " Makes a Racket module based on `racket/bare-bones' and the given
    Racket list of top-level module expressions. We create the module
    by evaluating a Racket `(module ...) form in the main namespace of
    Arc. The resulting module will have a gensym for a name (even if
    Racket's `current-module-declare-name' would have overridden
    that), and Racket's `compile-enforce-module-constants' parameter
    will be `#f' while the module is being compiled, so that its
    module-level variables can be redefined or assigned to later on. "
  (let name uniq.anon-module-prefix*
    (let expr ($.list 'module name (ac-denil ''racket/bare-bones)
                (cons racket/bare-bones--plain-module-begin
                  racket-module-body))
      (w/current-ns (anchor-ns)
        (let compiled (parameterize
                          $.compile-enforce-module-constants scheme-f
                        $.compile.expr)
          (parameterize $.current-module-declare-name scheme-f
            $.eval.compiled))))
    (pathed-rmodule (anchor-empty-rns) (ac-denil `',name))))

(def make-simple-rmodule binds
  (make-bare-bones-rmodule:ac-denil:mappend
    (fn ((var val))
      (let box $.box.val
        `((,racket/bare-bones--define-customvar ,var
            ,(embed-racket/bare-bones:fn () $.unbox.box)
            ,(embed-racket/bare-bones
               ; NOTE: We would use [] syntax, but that would try to
               ; ssexpand 'set-box!.
               (fn (val) (($ set-box!) box val))))
          (,racket/bare-bones--provide ,var))))
    pair.binds))

(def make-simple-module binds
  (module-racketarc:apply make-simple-rmodule
    (mappend [list (global-arcracket _.0) _.1] pair.binds)))

(mac simple-rmod binds
  `(make-simple-rmodule ,@(mappend [do `(',_.0 ,_.1)] pair.binds)))

(mac simple-mod binds
  `(make-simple-module ,@(mappend [do `(',_.0 ,_.1)] pair.binds)))

(def make-sub-rmodule (rmodule var-test)
  (let (rns path) (rep rmodulify.rmodule)
    (make-bare-bones-rmodule:ac-denil
      `((,racket/bare-bones--require ,path)
        ,@(map [do `(,racket/bare-bones--provide ,_)]
               (keep var-test rmodule-keys.rmodule))))))

(def make-renaming-rmodule (rmodule renamer)
  (let (rns path) (rep rmodulify.rmodule)
    (make-bare-bones-rmodule:ac-denil
      `((,racket/bare-bones--require ,path)
        ,@(map [do `(,racket/bare-bones--provide
                      (rename ,_ ,renamer._))]
               rmodule-keys.rmodule)))))


(defextend rnsify (x) (isa x 'rmodule)
  (let (rns path) (rep instantiate-rmodule.x)
    (w/current-rns rns $.module->namespace.path)))

(defextend nsify (x) (isa x 'module)
  (ns-racketarc:rnsify module-arcracket.x))


(def expand-w/rmodule (rmodule body)
  (zap instantiate-rmodule rmodule)
  (let (rns path) rep.rmodule
    ; We introduce local variables corresponding to the module's
    ; exports so that the Arc compiler doesn't use 'ac-global-name for
    ; those variables within 'body.
    `(with ,(mappend [do `(,_ nil)] rmodule-keys.rmodule)
       ($:local-require ,path)
       ,@body)))

(def expand-w/module (module body)
  (zap module-arcracket:modulify module)
  (zap make-sub-rmodule module could-global-racketarc)
  (zap make-renaming-rmodule module global-racketarc)
  (expand-w/rmodule module body))

(mac w/rmodule (rmodule . body)
  " Evaluates `rmodule' at expansion time and uses Racket's
    `local-require' to require the resulting module in a local
    scope for `body'. "
  (expand-w/rmodule eval.rmodule body))

(mac w/module (module . body)
  " Evaluates `module' at expansion time and uses Racket's
    `local-require' to require the Arc variables of the resulting
    module in a local scope for `body'. "
  (expand-w/module eval.module body))


(= modecule-var* 'val)  ; This is an Arc variable name.

(def view-modecule (module var)
  (annotate 'modecule (list module var)))

(def make-modecule (val (o var modecule-var*))
  (view-modecule (make-simple-module var val) var))




(def rns-var (var (o default-rns current-rns))
  (case type.var
    sym      (annotate 'rns-var (list var rnsify.default-rns))
    rns-var  var
             (err:+ "Can't rns-var " (tostring write.var))))

(withs (racket-stx [let (var rns) (rep rns-var._)
                     (w/current-rns rns
                       $.namespace-symbol->identifier.var)]
        (top set app datum quote-stx define-syntax)
          (map racket-stx
            '(#%top set! #%app #%datum quote define-syntax))
        embed [$.list app (cons datum (fn () _))])

  (def rns-get (var (o rns current-rns))
    " Gets a variable from a Racket namespace by evaluating it in
      Racket. Actually, it's sent through Racket's 'expand-to-top-form
      so that we can use the core #%top form if necessary rather than
      relying on the namespace itself to have one. "
    (let (var rns) (rep:rns-var var rns)
      (w/current-rns rns
        (let expanded $.expand-to-top-form.var
          ; If the expanded form is an identifier and its binding is
          ; #f (signifying a top-level binding), we use the core #%top
          ; form. Otherwise, we just evaluate it as-is.
          ($.eval:if (and $.identifier?.expanded
                          (no $.identifier-binding.expanded))
            ($.cons top expanded)
            expanded)))))

  (def ns-get (var (o ns current-ns))
    " Gets a variable from a namespace by evaluating it in Racket.
      Actually, it's sent through Racket's 'expand-to-top-form so that
      we can use the core #%top form if necessary rather than relying
      on the namespace itself to have one. "
    (rns-get global-arcracket.var ns-arcracket.ns))

  (def rns-set (var val (o rns current-rns))
    " Sets a variable in a Racket namespace using Racket's 'set!. "
    (let (var rns) (rep:rns-var var rns)
      ($.arc-exec ($.list set var embed.val) rns))
    val)

  (def ns-set (var val (o ns current-ns))
    " Sets a variable in a namespace using Racket's 'set!. "
    (rns-set global-arcracket.var val ns-arcracket.ns))

  (def rns-ownspace-set (var val (o rns current-rns))
    " Sets a top-level variable in a Racket namespace without changing
      the corresponding identifier mapping to point to that
      variable. "
    (let (var rns) (rep:rns-var var rns)
      (($ namespace-set-variable-value!) var val scheme-f rnsify.rns))
    val)

  (def ns-ownspace-set (var val (o ns current-ns))
    " Sets a top-level variable in a namespace without changing the
      corresponding identifier mapping to point to that variable. "
    (rns-ownspace-set global-arcracket.var val ns-arcracket.ns))

  (def rns-set-own (var val (o rns current-rns))
    " Sets a top-level variable in a Racket namespace and changes the
      corresponding identifier mapping to point to that variable. "
    (let (var rns) (rep:rns-var var rns)
      (($ namespace-set-variable-value!) var val scheme-t rnsify.rns))
    val)

  (def ns-set-own (var val (o ns current-ns))
    " Sets a top-level variable in a namespace and changes the
      corresponding identifier mapping to point to that variable. "
    (rns-set-own global-arcracket.var val ns-arcracket.ns))

  (def rns-set-renamer (observing-var
                        canonical-var (o canonical-rns current-rns))
    " Changes an identifier mapping in a Racket namespace to point to
      a rename transformer. "
    (let (observing-var observing-rns) (rep rns-var.observing-var)
      (ret transformer ($.make-rename-transformer:racket-stx:rns-var
                         canonical-var canonical-rns)
        (w/current-rns observing-rns
          ($.arc-exec:$.list
            define-syntax observing-var embed.transformer)))))

  (def ns-set-renamer (observing-var
                       canonical-var (o canonical-ns current-ns))
    " Changes an identifier mapping in a namespace to point to a
      rename transformer. "
    (rns-set-renamer global-arcracket.observing-var
      global-arcracket.canonical-var ns-arcracket.canonical-ns))

  (def rns-set-modecule (var modecule (o rns current-rns))
    (withs ((var rns) (rep:rns-var var rns)
            (mod modecule-var) rep.modecule
            (mod-rns path) (rep module-arcracket.mod))
      (w/current-rns rns
        ($.namespace-require:$.list 'rename
          path var global-arcracket.modecule-var)))
    modecule)

  (def ns-set-modecule (var modecule (o ns current-ns))
    (rns-set-modecule global-arcracket.var modecule ns-arcracket.ns))
  )

(defcall rns (self var)
  (rns-get var self))

(defcall ns (self var)
  (ns-get var self))

(defextend sref (self val . args) (and (isa self 'rns) single.args)
  (rns-set car.args val self))

(defextend sref (self val . args) (and (isa self 'ns) single.args)
  (ns-set car.args val self))

(defcall rmodule (self var)
  (rns-get var self))

(defcall module (self var)
  (ns-get var self))

(defextend sref (self val . args) (and (isa self 'rmodule) single.args)
  (rns-set car.args val self))

(defextend sref (self val . args) (and (isa self 'module) single.args)
  (ns-set car.args val self))


(def make-ns binds
  (ret result (anchor-empty-ns)
    (each (var val) pair.binds
      (ns-set-own var val result))))

(def make-base-ns binds
  (ret result ($.make-base-namespace)
    (each (var val) pair.binds
      (ns-set-own var val result))))

(mac nsobj binds
  `(make-ns ,@(mappend [do `(',_.0 ,_.1)] pair.binds)))

(def rns-keys (rns)
  (copy:$.namespace-mapped-symbols rnsify.rns))

(def ns-keys (ns)
  (safely-map-global-racketarc:rns-keys ns-arcracket.ns))
