; toy module system using the : syntax.

; (importmodule mod1) imports the file "mod1.arc" in the libdir* directory.
; (imp mod1 mod2 mod3) imports all three modules sequentially.
; (impas newname mod1) imports mod1 as newname.
; (impas nil mod1) imports everything in mod1 without qualifiers.
;if mod1 has 
;
;(def add1 (x)
;  "toy example"
;  (+ 1 x))
; 
;then we can do
; (imp mod1)
; (mod1:add1 5) => 6
;
;or
;
; (impas mymod mod1)
; (mymod:add1 5) => 6
;
;or
;
; (impas nil mod1)
; (add1 5) => 6
;

(= libdir* "lib/") ; not sure if I should be reusing the lib/ dir for this

(let defaultas (uniq)
  (mac importmodule (module (o as defaultas)) 
    (w/uniq (oldsafeset strmodname actualname) 
      `(with (,oldsafeset safeset 
              ,strmodname (coerce ',module 'string)
              ,actualname (coerce (if (is ',defaultas ',as) ',module ',as) 'string))
         (set safeset (annotate 'mac
            (fn (var val)
              (let fullyqualified 
                   (if ',as ; if they passed nil for as, then we don't qualify at all
                       (coerce (+ ,actualname "  " (coerce var 'string)) 'sym) 
                       var)
                `(do
                   (if (bound ',fullyqualified)
                      (do (disp "*** redefining ")
                          (disp ',fullyqualified)
                          (writec #\newline)))
                    (set ,fullyqualified ,val))))))
         (load (+ libdir* ,strmodname ".arc"))
         (set safeset ,oldsafeset)
         (when ',as
           (mac ,(if (is defaultas as) module as as (uniq)) libform 
             ; the above rigamarole because Arc doesn't like (mac nil ...) even if never used
             (let fullyqualified 
                  (if ',as ; if they passed nil for as, then we don't qualify at all
                      (coerce (+ ,actualname "  " (coerce (caar libform) 'string)) 'sym) 
                      (caar libform))
               `(,fullyqualified ,@(cdr (car libform))))))))))

(mac imp modules
  (let todo ()
    (each module (rev modules)
      (push (list 'importmodule module) todo))
  `(do ,@todo)))

(mac impas (as module)
  `(importmodule ,module ,as))

(mac import (module) 
  " plain import of a module into the standard name space "
  `(importmodule ,module nil))


; Possible TODO
; 
; If we had (export symbol1 symbol2 ...) then we could qualify the name differently 
; for symbols in the list, such that anything not in the list would remain private.  
; Seems easy enough.  Maybe tomorrow. :)
; 
; Some other ideas:
;(impfrom mod4 this that other) 
; imports only this, that, and other from mod4, even if other things are exported
; 
;(impas mymod mod4 this that other)
; like impas, but only these...
;(impas nil mod4 this that other)
; turns off prefixing entirely for this module, and imports this, that, and other directly,
; which is just like (load ...) except for the export checking, if we have that.
; 
; ISSUES
; 
; This doesn't close over the module being imported, and therefore doesn't 
; do anything with variables explicitly set global with = or set.  As far
; as I can see, a module system that did close over modules would have to 
; have additional support in the interpreter, rather than being implementable 
; directly in Arc. 
; 
; OTHER
; 
; written by Randall Randall <randall@randallsquared.com> 2008-02-03
; same license as Arc for compatibility: Perl Artistic 2
; also released into the public domain.
; 
; 2008-02-04 Fixed helper macro bug pointed out by greatness on arclanguage.org
; 
