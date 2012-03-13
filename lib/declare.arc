; by twilightsentry@gmail.com

(= declare-fns* (table))

(defs decl-idfn (old new args) new
      decl-bool (old new args) (no:no new))

(def declaration (key (o setfn decl-idfn) (o default))
  (= declare-fns*.key  setfn
     declarations*.key default))

(let mklist (fn (x)
              (check x alist list.x))
  (def declare (key val)
    (let (k . args) mklist.key
      (iflet f declare-fns*.k
             (zap f declarations*.k val args)
             declerr.key))))

(def decl (key)
  (if declare-fns*.key
      declarations*.key
      declerr.key))

(= declerr [err "Unknown declaration: " _])

(map [declaration _ decl-bool]
     '(atstrings direct-calls explicit-flush))
