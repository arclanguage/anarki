; extend by CatDancer <cat@catdancer.ws> (http://hacks.catdancer.ws)
; license: Public domain (http://creativecommons.org/licenses/publicdomain/)

; CHANGELOG
; 2010-02-15 Mark Huetsch <markhuetsch@gmail.com>
;  + moved to simpler extend from http://awwx.ws/extend0.arc simply
;    because the old versions don't seem to work with aw's new patches
;    and I don't have time to debug them.
; 2009-08-20: Michael Arntzenius <daekharel@gmail.com>
;  + reloading the file will no longer wipe the extensions table.
;  + refactored 'extend macro into a function 'extend-add, a function
;    'extend-fn, a macro 'extend-ensure, and a macro 'extend.
;  + remove unnecessary let in 'extend-wrap's afn
;  + add 'extend-pull fn, 'unextend macro

;(unless (and bound!extensions* extensions*)
;  (= extensions* (table)))
;
;(defmemo extend-fn (name)
;  (fn args
;    ((afn (((label (test func)) . rest))
;       (if (or (no test) (apply test args))
;             (apply func args)
;           (self rest)))
;     (or (extensions* name)
;         (err "no extension defined for" name)))))
;
;(mac extend-ensure (name)
;  `(do
;     (unless (extensions* ',name)
;       (= (extensions* ',name) `((original (nil ,,name)))))
;    (= ,name (extend-fn ',name))))
;
;(def extend-add (name label test func)
;  (aif (assoc label extensions*.name)
;        (do (prn "*** redefining " name " extension " label)
;            (= (cadr it) (list test func)))
;      (push `(,label (,test ,func)) extensions*.name)))
;
;(mac extend (name label test func)
; `(do1 (extend-ensure ,name)
;       (extend-add ',name ',label ,test ,func)))
;
;(def extend-pull (name label)
;  (prn "*** undefining " name " extension " label)
;  (pull [is car._ label] extensions*.name))
;
;(mac unextend (name label)
;  `(do (extend-pull ',name ',label) ,name))
;
;; original 'extend from extend0.arc
;(mac extend (name label test func)
;   `(do (unless (extensions* ',name)
;          (= (extensions* ',name) `((original (nil ,,name)))))
;        (aif (assoc ',label (extensions* ',name))
;              (do (prn "*** redefining " ',name " extension " ',label)
;                  (= (cadr it) (list ,test ,func)))
;              (push (list ',label (list ,test ,func)) (extensions* ',name)))
;        (= ,name (fn args
;                   ((afn (al)
;                      (let (label (test func)) (car al)
;                        (if (or (no test) (apply test args))
;                             (apply func args)
;                             (self (cdr al)))))
;                    (or (extensions* ',name)
;                        (err "no extension defined for" ',name)))))))

; TODO re-add unextend support
(mac extend (name arglist test . body)
  (w/uniq args
    `(let orig ,name
       (= ,name
          (fn ,args
            (aif (apply (fn ,arglist ,test) ,args)
                  (apply (fn ,arglist ,@body) ,args)
                  (apply orig ,args)))))))
