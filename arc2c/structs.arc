(def make-var (id uid)
	(listtab `((type var) (id ,id) (uid ,uid))))

(def avar (x)
	(and (isa x 'table) (is x!type 'var)))

(def make-macro (id expander)
	(listtab `((type macro) (id ,id) (expander ,expander))))

(def amacro (x)
	(and (isa x 'table) (is x!type 'macro)))

(def make-lit (subx val)
	(listtab `((type lit) (subx ,subx) (val ,val))))

(def alit (x)
	(and (isa x 'table) (is x!type 'lit)))

(def make-ref (subx var)
	(listtab `((type ref) (subx ,subx) (var ,var))))

(def aref (x)
	(and (isa x 'table) (is x!type 'ref)))

(def make-quote (subx)
	(listtab `((type quote) (subx ,subx))))

(def aquote (x)
	(and (isa x 'table) (is x!type 'quote)))

(def make-set (subx var)
	(listtab `((type set) (subx ,subx) (var ,var))))

(def aset (x)
	(and (isa x 'table) (is x!type 'set)))

(def make-cnd (subx)
	(listtab `((type cnd) (subx ,subx))))

(def acnd (x)
	(and (isa x 'table) (is x!type 'cnd)))

(def make-prim (subx op)
	(listtab `((type prim) (subx ,subx) (op ,op))))

(def aprim (x)
	(and (isa x 'table) (is x!type 'prim)))

(def make-app (subx)
	(listtab `((type app) (subx ,subx))))

(def anapp (x)
	(and (isa x 'table) (is x!type 'app)))

(def make-lam (subx params)
	(listtab `((type lam) (subx ,subx) (params ,params))))

(def alam (x)
	(and (isa x 'table) (is x!type 'lam)))

(def make-seq (subx)
	(listtab `((type seq) (subx ,subx))))

(def aseq (x)
	(and (isa x 'table) (is x!type 'seq)))


(def extend (bindings env)
  (+ bindings env))

(def lookup (id env)
	(if (no env)
		nil
		(let head (car env)
			(if (is head!id id)
				head
				(lookup id (cdr env))))))

(= seq-num* 0)

(def to-str (x)
	(coerce x 'string))

(def to-sym (x)
	(coerce x 'sym))

(def new-var (id)
	(++ seq-num*)
	(make-var id (to-sym (+ (to-str id) "@" (to-str seq-num*)))))

(def new-global (id)
	(make-var id id))

(def aglobal (var)
	(is var!id var!uid))

