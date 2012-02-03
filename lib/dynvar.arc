(require "lib/thread-cell.arc")

(mac with* (parms . body)
  (let uparms (mapeach (var val) (pair parms)
                (list var val (uniq (+ "old-" var)) (uniq (+ "new-" var))))
    `(with ,(mappendeach (var val old new) uparms
              `(,old ,var ,new ,val))
       ; we should arguably use dynamic-wind here, but pg's method seems to be
       ; to just use protect instead and never use continuations as anything but
       ; escape procedures.
       (protect
         (fn ()
           ; update the variables
           (= ,@(mappendeach (var val old new) uparms
                  `(,var ,new)))
           ,@body)
         (fn ()
           ; restore the variables
           (= ,@(mappendeach (var val old new) uparms
                  `(,var ,old))))))))

(mac let* (var val . body)
  `(with* (,var ,val) ,@body))

(mac withs* (parms . body)
  (if no.parms `(do ,@body)
    (let (var val . rest) parms
      `(let* ,var ,val (withs* ,rest ,@body)))))

; Thread-local vars to use with the above
(mac dynvar (name (o init-val))
  `(= ,name (thread-cell ,init-val)))

(mac dynvars vars
  `(do ,@(mapeach var vars
           (if atom.var `(dynvar ,var) `(dynvar ,@var)))))
