;; lib/module/python.arc: A small module system with Python-like semantics,
;; built by interfacing with mzscheme's "namespaces".

;; Written by Michael Arntzenius. Licensed under the WTFPL v2:

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                    Version 2, December 2004
;;
;; Copyright (C) 2004 Sam Hocevar
;;  14 rue de Plaisance, 75014 Paris, France
;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.
;;
;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO.

;; --------------------
;; The main point of entry is via the "use" macro. Examples:

;; (use test)
;;
;; This requires the module "test.arc" (see 'module-require), and binds it to 'test.
;;
;; Calling a module on an expression evaluates it in the namespace. So
;; (test!somefun) calls the function 'somefun defined in "test.arc". However,
;; you cannot use macros that "test.arc" defines.
;;
;; This limitation exists because ac.scm translates Arc into Scheme, and it must
;; know at translation time whether an expression is to be treated as a macro.
;; Unless it can show that it is a macro, it assumes it's not. Currently, it
;; only handles globally-bound symbols. I see no way to have ac.scm handle the
;; general case (that any expression could evaluate to a macro) without making
;; it a full-fledged interpreter, rather than a translator.

;; (use (as long-module/name alias))
;;
;; Requires the module "long-module/name.arc" and binds it to 'alias.

;; (use (all test))
;;
;; Requires the module "test.arc" and imports all symbols in it into our
;; namespace.

;; (use (from test sym1 sym2))
;;
;; Requires the module "test.arc", then imports 'sym1 and 'sym2 from it.

;; (use (as lib/foo foo)
;;      (from lib/foo mymac mymac2)
;;      (all lib/bar))
;;
;; Requires the modules "lib/foo.arc" and "lib/bar.arc". The former is bound to
;; 'foo, and 'mymac and 'mymac2 are imported from it. All symbols in the latter
;; are also imported. (In other words, use can handle multiple clauses.)
;; --------------------

;; Interface with namespaces in mzscheme
($
  (begin
    (xdef 'make-namespace
      (lambda ()
        (let ((ns (make-namespace 'initial)))
          (namespace-attach-module (current-namespace) "ac.scm" ns)
          (eval '(require "ac.scm") ns)
          ns)))

    (xdef 'eval-in
      (lambda (ns expr)
        (eval (ac (ac-denil expr) (list)) ns)))
    
    ;; Annotate uses 'type on its 'rep argument. Namespaces don't have an arc
    ;; type. Hence to annotate namespaces, we need this.
    (xdef 'annotate-force
      (lambda (type rep) (vector 'tagged type rep)))))

(def module-make ((o ns))
  (annotate-force 'module (or ns (make-namespace))))

(def module-syms (m)
  (map1
    [sym:cut _ 2]
    (keep [and (begins _ "__") (isnt "__nil" _)]
         (map1 string (($ namespace-mapped-symbols) (rep m))))))

;; Modules are evaluator functions
(defcall module (self expr)
  (eval-in self expr))

;; Loading files into modules
(def module-load-in (mod file (o hook))
  (push current-load-file* load-file-stack*)
  (= current-load-file* file)
  (or= hook idfn)
  (after
    (w/infile f file
      (whilet e (read f)
        (mod (hook e))))
    (do (= current-load-file* (pop load-file-stack*)) nil)))

;; Module table - maps keys (usually file paths) to modules
(= module-table* (table))

(def module-register (file mod)
  (= module-table*.file mod))

;; The "tl" (toplevel) module
(= module-tl* (module-make ($ (current-namespace))))

;; Creating modules
(def module-init (mod)
  ;; Attach & require the "ac.scm" module (module in the mzscheme sense)
  ($ (begin
       (namespace-attach-module (current-namespace) "ac.scm" ,(rep mod))
       (eval '(require "ac.scm") ,(rep mod))))
  
  ;; Transfer all symbols from the toplevel environment
  (module-transfer-syms
    module-tl* mod (map1 [list _ _] (module-syms module-tl*))))

(def module ()
  (let m (module-make)
    (module-init m)
    m))

;; Module loading, reloading, requiring
(def module-load (file)
  (let m (module)
    (module-load-in m file)
    m))

(def module-reload (mod file)
  (let m (module-load file)
    (seval `(vector-set! ,mod 2 ,(rep m)))
    mod))
  
(def module-spec->file (spec)
  (if (isa spec 'sym) (string spec ".arc")
      (isa spec 'string) spec
      (err "Unrecognized module specification")))

(def module-require (spec)
  (let file (module-spec->file spec)
    (or (module-table* file)
        (let m (module-load file)
          (module-register file m)
          m))))

;; Importing symbols from modules.
(def module-transfer-syms (src dest sym-pairs)
  (each (src-name dest-name) sym-pairs

    (if (is 0 (len (string src-name)))
        (prn src-name dest-name))
    
    (($ namespace-set-variable-value!)
     (sym:string "__" dest-name)
     
     (seval
       `(namespace-variable-value
          ',(sym:string "__" src-name)
          #t
          (not #t)
          ',(rep src)))
     ;; (($ namespace-variable-value)
     ;;  (sym:string "__" src-name) #t #f (rep src))
     #f
     (rep dest))))

(def module-import-syms (src syms)
  (module-transfer-syms
    src (module-make ($ (current-namespace)))
    (map1 [list _ _] syms)))

(def module-import-all (src)
  (module-import-syms src (module-syms src)))

;; "use" macro
(def module-xfrm-use-clause (head rest)
  (case head
    as (let (mod name)
         `(safeset ,name (module-require ',mod)))
    from (let (mod . syms) rest
           `(module-import-syms (module-require ',mod) ',syms))
    all (let (mod) rest
           ` (module-import-all (module-require ',mod)))
    (err "Unrecognized use-clause: " head)))

(mac use body
  `(do
     ,@(mapeach e body
         (if
           (isa e 'sym)
             `(safeset ,e (module-require ',e))
           (module-xfrm-use-clause (car e) (cdr e))))))
