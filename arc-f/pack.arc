; Author: Stefano Dissegna

; License: Do whatever you like with this

; package handling, lets you create and load libraries in a simple manner.
; a package is simply a directory. 
; files in the src sub-directory are loaded in name order.
; if your library needs some files that don't have to be loaded, you can put 
; them anywhere in the package directory except the src sub-directory
 
; Example:
; to pack the http-get library:
; >  (<pack>pack-lib 'http-get "" nil "lib/http-get/http-utils.arc" "lib/http-get/http-get.arc")
; then to load the library:
; > (<pack>use 'http-get)
; it will work if the directory http-get.pack is within the search path
; by default the search path contains only the working directory
; to add a new directory, for example "./lib", use
; > (<pack>add-path "./lib")
; the last path added has the highest priority
; it is possible to create a package that depends on other packages:
; > (<pack>pack-lib 'mylib "" '(http-get xml) "myfile.arc")
; this will build a package named mylib that loads the packages 'http-get
; and 'xml before loading itself
; Warning: when forcing package reloading, dependencies aren't forced

; 'use and 'pack-lib are to be used only for usage and delivery,
; not for library development
; to develop a library you first need to build the right directory structure 
; for it. To do this, you just need to use the funtion 'mk-stub:
; > (<pack>mk-stub 'my-lib)
; this will create a directory named "my-lib". Within this directory you will 
; find a file named "proj.arc". You need to modify this file adding a list 
; of dependencies and the files that make up your library. Now you can load
; the project definition:
; > (load "my-lib/proj.arc")
; To load the source files, use:
; > (<pack>proj-load)
; Now if you modify some files, to reload them you just need to use 'proj-load
; and only modified files will be loaded.
; To deliver your library, just use:
; > (<pack>deliver-library)
; this will create a library as a directory named "my-lib.pack".
; Remeber that if you modify proj.arc, you'll need to reload it manually.
; You should also pay attention to macros: if you modify a macro, you should
; reload every file that uses it, not just the modified files. 
; To reload every file, use:
; > (<pack>proj-load t)

; you can search through installed libraries using 'pack-query
; E.g. to find all the libraries that have something to do with http:
; > (pack-query ".*http.*")

(in-package pack)
(using <arc>v3)
(using <arc>v3-packages)
(using <files>v1)
(interface loading add-path use)
(interface project defproject proj-load deliver-library)
(interface query build-cache query)

; hold names of packages already loaded
(= loaded* (table))

; path were packages are searched
(= search-path* '("."))

(def add-path (path)
  "add path to the search path"
  (push path search-path*))

(def use (name (o force))
  "load a library or a project if it hasn't been already loaded"
  (let name (<arc>unpkg name)
    (when (or force (no (loaded* name)))
      (prn:string "Loading " name)
      (if (or (some [let path (file-join _ (string name ".pack"))
                      (when (dir-exists path)
                        (load-dir path))]
                    search-path*)
              (some [try-load-project _ name] search-path*))
         (do (= (loaded* name) t) 'done)
         (err:string "Couldn't find library or project " name)))))

(def load-dir (path)
  "load a library in a directory"
  (prn "Loading as a library")
  (let files (map [file-join path "src" _] (sort < (dir:file-join path "src")))
    ; if one file doesn't exist load will raise an error
    (each _ files
      (prn:string "Loading " _) 
      (load _)))
  t)

(def log10+1 (n)
  (+ 1 (coerce (/ (<arc>log n) (<arc>log 10)) 'int)))

(def int->str (n digits)
  "transforms a number into a string with at least given digits
   n must fit in digits and must be positive"
  (when (< n 0) (err "n must be positive!"))
  (let missing (- digits (log10+1 n))
    (when (< missing 0) (err "n doesn't fit!"))
    (tostring
      (for i 1 missing (pr "0"))
      (pr n))))

(def build-deps (deps out-dir)
  "create a file in out-dir named 0 that loads specified dependencies"
  (w/stdout (outfile:file-join out-dir "src" "0")
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
          (mkdir out) ; package directory
          (mkdir:file-join out "src"))) ; dir of source files
      (w/stdout (outfile:file-join out "desc")
        (write description))
      (when deps
        (build-deps deps out))
      (each f files
        (cp (car f) (file-join out "src" (cdr f))))))
  'done)

; integration with require
; based on an idea by AmkG
;(let old require
  ; doesn't work correctly if pack.arc is loaded more than once...
  (defm require ((t what sym))
    "require that automatically uses 'use when argument is a symbol
     !! should be handled  with (defm require ((t ..."
    (use what))
;    (if (is (type what) 'sym) (use what) (old what))))

; library development management

(= projects* (table)) ; table of active projects

(= current-proj* nil) ; current project

(deftem project
  name nil
  desc ""
  sources nil
  deps nil)

(def sources (proj)
  "return list of all the source files of the project
   the list is ordered in load order"
  proj!sources)

(def source-name (s)
  (car s))

(def source-date (s)
  "date of the last time the file was loaded"
  (cdr s))

(def source-new-date (s)
  "set date of the file to that of the filesystem"
  (= (cdr s) (source-real-date s)))

(def source-real-date (s)
  "date the source was modified"
  (mtime:source-name s))

(def mod-sources (proj)
  "return list of source files in project modified since the last time
   they were loaded"
  (keep [let d (source-date _)
          (or (no d) (< d (source-real-date _)))]
        (sources proj)))

(def proj-load ((o force nil) (o proj current-project*))
  "take a project name, require dependencies and load outdated sources"
  (let proj (projects* proj)
    (each d proj!deps (require d))
    (each f ((if force sources mod-sources) proj)
      (prn:string "Loading " (source-name f))
      (load (source-name f))
      (source-new-date f))))

(def try-load-project (path name)
  "if path/name/proj.arc exists then load it"
  (let project-file (string path "/" name "/proj.arc")
    (when (file-exists project-file)
      (prn "Loading as a project")
      (load project-file)
      (proj-load nil name)
      t)))

(def norm (x)
  (if (isa x 'sym) (<arc>unpkg x) x))

(def deliver-library ((o proj current-project*))
  "build a library out of given project name"
  (let proj (projects* proj)
    (apply pack-lib (unpkg proj!name) proj!desc (map norm proj!deps)
                    (map source-name (sources proj)))))

(mac defproject (name deps description . ordered-files)
  "define a project named name that requires the dependencies in deps
   and that is made of the given files
   make the project the current one
   TODO: !! for the moment path must be relative to arc.sh directory !!"
  (w/uniq sname
    `(let ,sname (<arc>unpkg ',name)
       (= (projects* ,sname) (inst 'project 'name ,sname 
                                            'desc ,description
                                            'deps ',deps
                                            'sources (map [cons _ nil] 
                                                          ',ordered-files)))
       (= current-project* ,sname))))

(def mk-stub (name (o parent "."))
  "make a stub for a project"
  (let d (file-join parent (string:unpkg name))
    (when (dir-exists d)
      (err "Directory already exists!"))
    (mkdir d)
    (w/stdout (outfile:file-join d "proj.arc")
      (prn)
      (prn "(in-package " (unpkg name) ")")
      (prn "(using <pack>project)")
      (prn)
      (prn "(defproject " (unpkg name) " () ; put here list of dependencies")
      (prn "  \"project description\"")
      (prn "  ; put here your files")
      (prn "  )")
      (prn ""))
    'done))

; local library db
; create a cache in the home directory
; the cache is just a file of strings
; every string is the concatenation of project name and project description

(= cache-dir* "~/.arc")

(def ensure-cache-dir ()
  (unless (dir-exists cache-dir*) (mkdir cache-dir*)))

(def build-cache ()
  "build a cache of installed packages"
  (ensure-cache-dir)
  (w/stdout (outfile:string cache-dir* "/cache")
    ; find all packages in the search path
    ; a directory ending in ".pack" is considered a package
    (each p search-path*
      (withs (match ".pack"
              lm (len match)
              packs (keep [and (> (len _) (len ".pack"))
                               (is (cut _ (- (len _) lm)) match)]
                          (dir p)))
        (each pack packs
          (write:string pack ":" #\newline
                        (readfile1:file-join p pack "desc")))))))

(def ensure-cache ()
  (unless (file-exists:file-join cache-dir* "cache")
    (build-cache)))

; straight from strings.arc
(= re-match (fn (x y) (no (no (<arc>$.regexp-match x y)))))

(def query (re)
  "search regular expression re in package names and descriptions
   incredibly slow for a large number of packages"
  (ensure-cache)
  ; !! readfile in arc-f gives a strange error: 
  ; Error: "Can't get reference #3(tagged <arc>mac #<procedure>)"
  ; substituted with readall:infile
  (let all (readfile (file-join cache-dir* "cache"))
    (each result (keep [re-match re _] all)
      (prn "Found: ")
      (prn result)
      (prn "---"))))

