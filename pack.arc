; package handling, lets you create and load libraries in a simple manner.
; a package is simply a directory. 
; files in the src sub-directory are loaded in name order.
; if your library needs some files that don't have to be loaded, you can put 
; them anywhere in the package directory except the src sub-directory
 
; Example:
; to pack the http-get library:
; >  (pack-lib 'http-get nil "lib/http-get/http-utils.arc" "lib/http-get/http-get.arc")
; then to load the library:
; > (use-pack 'http-get)
; it will work if the directory http-get.pack is within the search path
; by default the search path contains only the working directory
; to add a new directory, for example "./lib", use
; > (pack-add-path "./lib")
; the last path added has the highest priority
; it is possible to create a package that depends on other packages:
; > (pack-lib 'mylib '(http-get xml) "myfile.arc")
; this will build a package named mylib that loads the packages 'http-get
; and 'xml before loading itself
; Warning: when forcing package reloading, dependencies aren't forced

; hold names of packages already loaded
(= pack-loaded* (table))

; path were packages are searched
(= pack-search-path* '("."))

(def pack-add-path (path)
  "add path to the search path"
  (push path pack-search-path*))

(def use-pack (name (o force))
  "load a package if it hasn't been already loaded"
  (when (or force (no (pack-loaded* name)))
    (if (some [let path (string _ "/" name ".pack")
                 (when (dir-exists path)
                   (pack-load-dir path))]
              pack-search-path*)
       (do (= (pack-loaded* name) t) 'done)
       (err:string "Couldn't find library " name))))

(def pack-load-dir (path)
  "load a library in a directory"
  (let files (map [string path "/src/" _] (sort < (dir:string path "/src")))
    ; if one file doesn't exist load will raise an error
    (each _ files (load _)))
  t)

(def int->str (n digits)
  "transforms a number into a string with at least given digits
   n must fit in digits and must be positive"
  (when (< n 0) (err "n must be positive!"))
  (let missing (- digits (quotient n 10))
    (when (< missing 0) (err "n doesn't fit!"))
    (tostring
      (for i 1 missing (pr "0"))
      (pr n))))

(def pack-build-deps (deps out-dir)
  "create a file in out-dir named 0 that loads specified dependencies"
  (w/stdout (outfile:string out-dir "/src/0")
    (each dep deps 
      (prn `(use-pack ',dep)))))

; TODO: add option to overwrite target directory?
(def pack-lib (name deps . file-lst)
  "create a library named name made of files in file-lst
   files will be loaded in the given order
   deps is a list of needed packages
   !! doesn't work for more than 1000 files"
  (let out (string name ".pack")
    (withs (num 1 ; 0 is reserved for dependencies loading
            files (map [cons _ (int->str (++ num) 10)] file-lst))
      (if (dir-exists out)
        (do
          (prn "!! Warning: package directory already exists!")
          (prn "!! Package may be broken"))
        (do
          (make-directory out) ; package directory
          (make-directory:string out "/src"))) ; dir of source files
      (when deps
        (pack-build-deps deps out))
      (each f files
        (cp (car f) (string out "/src/" (cdr f))))))
  'done)

; integration with require
; based on an idea by AmkG
(let old require
  ; doesn't work correctly if pack.arc is loaded more than once...
  (def require (what)
    "require that automatically uses use-pack when argument is a symbol"
    (if (is (type what) 'sym) (use-pack what) (old what))))
