; On-line help system
(def ppr-source (name)
  " Pretty-prints the source code of the function `name'.
    Is a function, so `name' is evaluated.
    See also: [[src]] "
  ((only [prn "(from \"" _ "\")"]) source-file*.name)
  (ppr source*.name))

(mac src (name)
  " Pretty prints the source code of the function `name'. 
    Is a macro, so `name' is not evaluated.
    See also: [[ppr-source]] "
  `(ppr-source ',name))

(mac help ((o name 'help))
  " Prints the documentation of the given symbol. To use, type
    (help symbol) ; you may also use (help \"string\") to search
    all documentation for that string. "
  (if (isa name 'sym) `(do (pr ,(helpstr name)) nil)
      (isa name 'string) `(helpsearch ,name)
      `(help)))

(def helpsearch (str)
  " Prints all symbols whose documentation matches or partly matches `str'. "
  (prall (helpsearch-core str) "Related symbols:\n" "\n")
  (prn)
  nil)

(def helpsearch-core (str)
  " Returns a list of symbols whose documentation matches or partly matches 
    `str'. "
  (zap downcase str)
  (let part-match [findsubseq str (downcase:string _)]
    (sort < 
      (accum add
        (each (name doc) help* 
          (when (or (part-match name) (part-match doc)
                    (only.part-match (source-file* name)))
            (add name)))))))

(def helpstr (name (o verbose t))
  " Returns a help string for the symbol `name'. "
  (tostring
    (withs (value (errsafe:eval name)
            kind  (type value)
            doc   (help* name))
      (when (or value doc)
        (pr "[" kind "]")
        (apply pr (n-of (- 4 (len:string kind)) " "))
        (write (aif sig.name (cons name it)
                    (in kind 'fn 'mac) (list name)
                    name))
        (prn)
        (when verbose
          (prn (or doc (string name " is not documented."))))))))

(def fns ((o test))
  " Print sigs for macros & functions whose names (as symbols) match `test'.
    If `test' is a function, it is used as a predicate.
    Otherwise, names of which `(string test)' is a prefix pass. "
  (let test (if (isa test 'fn) test
              (let pfx string.test [begins string._ pfx]))
    (each f (sort < (keep test keys.sig))
      (pr (helpstr sym.f nil)))))
