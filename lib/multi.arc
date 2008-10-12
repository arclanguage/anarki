;; lib/multi.arc: simple multimethods for arc
;; Author: Michael Arntzenius
;; License: Do what you want, but it's not my fault!

;; This is modeled after, but not a direct port of, Clojure's defmulti &
;; related. See http://clojure.org/multimethods.

(mac multi (name dispatcher (o default-method))
  (let table-sym (sym:string name "-methods")
    (w/uniq (args fun gdisp)
      `(let ,gdisp ,dispatcher
         ;; we don't create a new method table unless: 1. this is the first time
         ;; we're being loaded, or 2. the table has been wiped.
         (unless (and (bound ',table-sym) ,table-sym)
           (set ,table-sym (table)))
         (def ,name ,args
           (apply
             (or (,table-sym (apply ,gdisp ,args))
               ,(or default-method
                  `(err "no matching method found for multimethod:" ',name)))
             ,args))))))

(mac method (name dispatch-value . fn-body)
  (let table-sym (sym:string name "-methods")
    (w/uniq (fun)
      `(let ,fun (fn ,@fn-body)
         (= (,table-sym ,dispatch-value) ,fun)))))

;; Convenience macro for dispatching based on the value the first argument
;; returns when passed a certain argument. Usually used for dispatching based on
;; the value of some key in a table, hence the name.
(mac multi-keyed (name key (o default-method))
  `(multi ,name (fn (a . r) (a ,key)) ,default-method))
