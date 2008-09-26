; package handling, lets you create and load libraries in a simple manner
; a package is a single file or a directory containing the files of 
; the library. files from the library's directory are loaded in name order

; Example:
; to pack the http-get library:
; >  (pack-lib 'http-get "lib/http-get/http-utils.arc" "lib/http-get/http-get.arc")
; then to load the library:
; > (use-pack 'http-get)
; it will work if the directory http-get.pack is within the search path
; by default the search path contains only the working directory
; to add a new directory, for example "./lib", use
; > (pack-add-path "./lib")
; the last path added has the highest priority

; TODO: add support for dependencies
; TODO: currently uses unix specific commands cp and mkdir
;       should make it work also under other OSes

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
                 (if
                   (dir-exists path) ; directory ?
                     (pack-load-dir path)
                   (file-exists path)
                     (pack-load-file path))]
              pack-search-path*)
       (do (= (pack-loaded* name) t) 'done)
       (err:string "Couldn't find library " name))))

(def pack-load-dir (path)
  "load a library in a directory"
  (let files (map [string path "/" _] (sort < (dir path)))
    (each _ files (load _)))
  t)

(def pack-load-file (path)
  "load a single file library"
  (load path)
  t)

(def int->str (n digits)
  "transforms a number into a string with at least given digits
   n must fist in digits and must be positive"
  (when (< n 0) (err "n must be positive!"))
  (let missing (- digits (quotient n 10))
    (when (< missing 0) (err "n doesn't fit!"))
    (tostring
      (for i 1 missing (pr "0"))
      (pr n))))

(def pack-lib (name . file-lst)
  "create a library named name made of files in file-lst
   files will be loaded in the given order
   !! doesn't work for more than 1000 files"
  (let out (string name ".pack")
    (if (is (len file-lst) 1)
      (system:string "cp " (car file-lst) " " out)
      (withs (num 0
              files (map [cons _ (int->str (++ num) 10)] file-lst))
        (unless (dir-exists out)
          (system:string "mkdir " out))
        (each f files
          (system:string "cp " (car f) " " out "/" (cdr f)))))))
