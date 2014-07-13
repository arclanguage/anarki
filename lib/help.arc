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
  (let part-match [posmatch str (downcase:string _)]
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
        (write (aif sig*.name (cons name it)
                    (in kind 'fn 'mac) (list name)
                    name))
        (prn)
        (when verbose
          (prn (or doc (string name " is not documented.")))
          (awhen (examples* name)
            (w/line-length 20
              (prn)
              (prn "Examples:")
              (each (expr expected) pair.it
                (pr "  arc> ")
                (ppr-main expr 7 t)
                (prn)
                (when (~is '_ expected)
                  (if (caris expected 'valueof)
                    (print-example-session expr (eval expected.1))
                    (print-example-session expr expected)))))))))))

(def print-example-session (expr expected)
  (pr "  ")
  (print-like-repl expected)
  (if (~iso expected eval.expr)
    (pr " <-- this seems outdated"))
  (prn))

(def print-like-repl (x)
  (if (isa x 'string)
    (write x)
    (pr x)))
