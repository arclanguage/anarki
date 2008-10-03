; Author: Stefano Dissegna

; License: Do whatever you like with this

; package handling, lets you create and load libraries in a simple manner.
; a package is simply a directory. 
; files in the src sub-directory are loaded in name order.
; if your library needs some files that don't have to be loaded, you can put 
; them anywhere in the package directory except the src sub-directory
 
; Example:
; to pack the http-get library:
; >  (pack-lib 'http-get "" nil "lib/http-get/http-utils.arc" "lib/http-get/http-get.arc")
; then to load the library:
; > (use-pack 'http-get)
; it will work if the directory http-get.pack is within the search path
; by default the search path contains only the working directory
; to add a new directory, for example "./lib", use
; > (pack-add-path "./lib")
; the last path added has the highest priority
; it is possible to create a package that depends on other packages:
; > (pack-lib 'mylib "" '(http-get xml) "myfile.arc")
; this will build a package named mylib that loads the packages 'http-get
; and 'xml before loading itself
; Warning: when forcing package reloading, dependencies aren't forced

; 'use-pack and 'pack-lib are to be used only for usage and delivery,
; not for library development
; to develop a library you first need to build the right directory structure 
; for it. To do this, you just need to use the funtion 'proj-mk-stub:
; > (proj-mk-stub 'my-lib)
; this will create a directory named "my-lib". Within this directory you will 
; find a file named "proj.arc". You need to modify this file adding a list 
; of dependencies and the files that make up your library. Now you can load
; the project definition:
; > (load "my-lib/proj.arc")
; To load the source files, use:
; > (proj-load)
; Now if you modify some files, to reload them you just need to use 'proj-load
; and only modified files will be loaded.
; To deliver your library, just use:
; > (proj-deliver-package)
; this will create a library as a directory named "my-lib.pack".
; Remeber that if you modify proj.arc, you'll need to reload it manually.
; You should also pay attention to macros: if you modify a macro, you should
; reload every file that uses it, not just the modified files. 
; To reload every file, use:
; > (proj-load t)

; you can search through installed libraries using 'pack-query
; E.g. to find all the libraries that have something to do with http:
; > (pack-query ".*http.*")

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
    (prn:string "Loading " name)
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
    (each _ files
      (prn:string "Loading " _) 
      (load _)))
  t)

(def log10+1 (n)
  (+ 1 (coerce (/ (log n) (log 10)) 'int)))

(def int->str (n digits)
  "transforms a number into a string with at least given digits
   n must fit in digits and must be positive"
  (when (< n 0) (err "n must be positive!"))
  (let missing (- digits (log10+1 n))
    (when (< missing 0) (err "n doesn't fit!"))
    (tostring
      (for i 1 missing (pr "0"))
      (pr n))))

(def pack-build-deps (deps out-dir)
  "create a file in out-dir named 0 that loads specified dependencies"
  (w/stdout (outfile:string out-dir "/src/0")
    (each dep deps 
      (write `(require ,(if (is (type dep) 'sym) `',dep dep)))
      (prn))))

; TODO: add option to overwrite target directory?
(def pack-lib (name description deps . file-lst)
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
      (w/stdout (outfile:string out "/desc")
        (write description))
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

; library development management

(= projects* (table)) ; table of active projects

(= current-proj* nil) ; current project

(deftem project
  name nil
  desc ""
  sources nil
  deps nil)

(def proj-sources (proj)
  "return list of all the source files of the project
   the list is ordered in load order"
  proj!sources)

(def proj-source-name (s)
  (car s))

(def proj-source-date (s)
  "date of the last time the file was loaded"
  (cdr s))

(def proj-source-new-date (s)
  "set date of the file to that of the filesystem"
  (= (cdr s) (proj-source-real-date s)))

(def proj-source-real-date (s)
  "date the source was modified"
  (mtime:proj-source-name s))

(def proj-mod-sources (proj)
  "return list of source files in project modified since the last time
   they were loaded"
  (keep [let d (proj-source-date _)
          (or (no d) (< d (proj-source-real-date _)))]
        (proj-sources proj)))

(def proj-load ((o force nil) (o proj current-project*))
  "take a project name, require dependencies and load outdated sources"
  (let proj (projects* proj)
    (each d proj!deps (require d))
    (each f ((if force proj-sources proj-mod-sources) proj)
      (prn:string "Loading " (proj-source-name f))
      (load (proj-source-name f))
      (proj-source-new-date f))))

(def proj-deliver-package ((o proj current-project*))
  "build a library out of given project name"
  (let proj (projects* proj)
    (apply pack-lib proj!name proj!desc proj!deps (map proj-source-name 
                                                       (proj-sources proj)))))

(mac defproject (name deps description . ordered-files)
  "define a project named name that requires the dependencies in deps
   and that is made of the given files
   make the project the current one
   TODO: !! for the moment path must be relative to arc.sh directory !!"
  (w/uniq sname
    `(let ,sname ',name
       (= (projects* ,sname) (inst 'project 'name ,sname 
                                            'desc ,description
                                            'deps ',deps
                                            'sources (map [cons _ nil] 
                                                          ',ordered-files)))
       (= current-project* ,sname))))

(def proj-mk-stub (name (o parent "."))
  "make a stub for a project"
  (let d (string parent '/ name)
    (when (dir-exists d)
      (err "Directory already exists!"))
    (make-directory d)
    (w/stdout (outfile:string d '/ "proj.arc")
      (prn "(defproject " name " () ; put here list of dependencies")
      (prn "  \"project description\"")
      (prn "  ; put here your files")
      (prn "  )")
      (prn ""))
    'done))

; local library db
; create a cache in the home directory
; the cache is just a file of strings
; every string is the concatenation of project name and project description

(= pack-cache-dir* "~/.arc")

(def pack-ensure-cache-dir ()
  (unless (dir-exists pack-cache-dir*) (make-directory pack-cache-dir*)))

(def pack-build-cache ()
  "build a cache of installed packages"
  (pack-ensure-cache-dir)
  (w/stdout (outfile:string pack-cache-dir* '/ "cache")
    ; find all packages in the search path
    ; a directory ending in ".pack" is considered a package
    (each p pack-search-path*
      (withs (match ".pack"
              lm (len match)
              packs (keep [and (> (len _) (len ".pack"))
                               (is (cut _ (- (len _) lm)) match)]
                          (dir p)))
        (each pack packs
          (write:string pack ":" #\newline
                        (readfile1:string p '/ pack "/desc")))))))

(def pack-ensure-cache ()
  (unless (file-exists:string pack-cache-dir* '/ "cache")
    (pack-build-cache)))

(def pack-query (re)
  "search regular expression re in package names and descriptions
   incredibly slow for a large number of packages"
  (pack-ensure-cache)
  (let all (readfile (string pack-cache-dir* '/ "cache"))
    (each result (keep [re-match re _] all)
      (prn "Found: ")
      (prn result)
      (prn "---"))))
