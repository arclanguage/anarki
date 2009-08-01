; User-definable calling for given types via coerce* extension
(def set-coercer (to from fun)
  " Makes 'fun the coercion function from 'from to 'to.
    See also: [[defcoerce]] "
  (let cnv (or coerce*.to (= coerce*.to (table)))
    (= cnv.from fun)))

(mac defcoerce (to from parms . body)
  " Defines the coercion function from 'from to 'to.
    See also: [[set-coercer]] [[defcall]] "
  `(set-coercer ',to ',from (fn ,parms ,@body)))

(mac defcall (type-name parms . body)
  " Defines the calling function for type 'type-name.
    See also: [[defcoerce]] "
  (w/uniq (fnobj args)
    `(defcoerce fn ,type-name (,fnobj)
       (fn ,args (apply (fn ,parms ,@body) ,fnobj ,args)))))
