;; lib/module/python.arc: A small module system with Python-like semantics.
;; Implemented using mzscheme's "namespaces".

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
;; The main point of entry is via the 'use macro. Examples:

;; (use test)
;;
;; This requires the module "test.arc" (see 'require-module), binds it to
;; 'test*, and binds its macro (see 'module->mac) to 'test. Calling a module on
;; an expression evaluates it in that modules' namespace. Calling a module's
;; macro on an expression does the same, except quoted. Moreover, by calling a
;; module's macro, you can use macros defined in that module.
;;
;; For example, (test*!somefun) and (test.somefun) both call the function
;; 'somefun defined in "test.arc" with no arguments, while (test.somemac) calls
;; the macro 'somemac defined in "test.arc" with no arguments.
;;
;; Note that mixing macros and modules is tricky business. If I have a macro
;; which expands to a call to some function defined in the same file, and I load
;; the file as a module and use the macro, it will fail, because macroexpansion
;; is entirely syntactic and will not capture the namespace in which the macro
;; was defined. This points to the need for a module system better integrated
;; with the language itself.

;; (use (as long/and/nested/module/name alias alias*))
;;
;; The same as (use long/and/nested/module/name), except that the module is
;; bound to 'alias*, and the module macro is bound to 'alias. 'alias* may be
;; omitted, in which case it will be whatever 'alias is, with a star appended.

;; (use (all test))
;;
;; Requires the module "test.arc" and imports all symbols in it into our
;; namespace.

;; (use (from test sym1 sym2))
;;
;; Requires the module "test.arc" and imports 'sym1 and 'sym2 from it.

;; 'use can also handle multiple clauses:
;;
;; (use (as lib/foo foo)
;;      (from lib/foo mymac mymac2)
;;      (all lib/bar))
;; --------------------

(require "lib/dynvars.arc")

(dynvars load-files&)

;; Modules are a thin layer over namespaces in mzscheme.
(def make-namespace ()
  ($ 
    (let ((ns (make-namespace 'initial)))
      (namespace-attach-module (current-namespace) "ac.scm" ns)
      (eval '(require "ac.scm") ns)
      ns)))

(def namespace->module ((o ns (make-namespace)))
  ($.vector 'tagged 'module ns))

(def module-syms (m)
  (map1
    [sym:cut _ 2]
    (keep [and (begins _ "__") (isnt _ "__nil")]
         (map1 string ($.namespace-mapped-symbols rep.m)))))

;; Modules are evaluator functions
(defcall module (self expr)
  ($ (eval (ac (ac-denil ',expr) (list)) ,self)))

;; Loading files into modules
(def load-in-module (mod file (o hook))
  (or= hook idfn)
  (slet (load-files&) file
    (w/infile f file
      (map1 mod:hook (readall f)))))

;; Module table - maps keys (usually file paths) to modules
(when (or (~bound 'module-table*) no.module-table*)
  (set module-table* (table)))

(def register-module (file mod)
  (set module-table*.file mod))

;; The "tl" (toplevel) module
(set module-tl*
  ($.vector 'tagged 'module ($.current-namespace)))

;; Creating modules
(def init-module (mod)
  ;; Transfer all symbols from the toplevel environment
  (transfer-syms module-tl* mod
    (map1 [list _ _] module-syms.module-tl*)))

(def make-module ()
  (let m (namespace->module)
    (init-module m)
    m))

;; Module loading, reloading, requiring
(def load-module (file)
  (let m (make-module)
    (load-in-module m file)
    m))

(def reload-module (mod file)
  (let m (load-module file)
    (vec-set mod 2 (rep m))
    mod))

(def module-spec->file (spec)
  (if (isa spec 'sym) (string spec ".arc")
      (isa spec 'string) spec
      (err "Unrecognized module specification")))

(def require-module (spec)
  (let file (module-spec->file spec)
    (or module-table*.file
        (let m load-module.file
          (register-module file m)
          m))))

;; Importing symbols from modules.
(def transfer-syms (src dest sym-pairs)
  (each (src-name dest-name) sym-pairs
    (($ namespace-set-variable-value!)
      (sym:string "__" dest-name)
      ($ (namespace-variable-value
           ',(sym:string "__" src-name)
           #t
           (not #t)
           ',(rep src)))
     #f
     (rep dest))))

(def import-syms (src syms)
  (transfer-syms
    src (namespace->module ($ (current-namespace)))
    (map1 [list _ _] syms)))

(def import-all (src)
  (import-syms src (module-syms src)))

;; Macro wrappers for modules
(def module->mac (mod)
  (annotate 'mac [mod _]))

;; "use" macro
(mac use-as (spec mac-name (o fun-name (sym:string mac-name "*")))
  `(do
     (safeset ,fun-name (require-module ',spec))
     (safeset ,mac-name (module->mac ,fun-name))))

(mac use body
  `(do
     ,@(mapeach clause body
         (if (isa clause 'sym) `(use-as ,clause ,clause)
           (let (head . rest) clause
             (case head
               as `(use-as ,@rest)
               from (let (mod . syms) rest
                      `(import-syms (require-module ',mod) ',syms))
               all (let (mod) rest
                     `(import-all (require-module ',mod)))
               (err "Unrecognized use-clause: " head)))))))
