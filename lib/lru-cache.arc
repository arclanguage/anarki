; augment a function with a cache of its most recent calls
(def lru (capacity f)
  (if (iso capacity 0)
    (err "lru with capacity 0 won't work"))
  (withs (values  list.nil  ; contains (args result) pairs least-recently-called first; first elem is always nil
          lastvalue  values
          ; todo: store nil values like in memo
          pointers  (table)  ; args -> cons cell *before* it in values
          evict  (fn ()
                   (when cdr.values
                     (let (args result) (pop cdr.values)
                       (wipe pointers.args))))
          insert  (fn (args-and-result)
                    (= cdr.lastvalue list.args-and-result)
                    (= (pointers args-and-result.0)
                       lastvalue)
                    (= lastvalue cdr.lastvalue)
                    (when (< capacity (len cdr.values))  ; todo: avoid len
                      (evict)))
          lookup  (fn (args)
                    (awhen pointers.args
                      (let args-and-result it.1
                        ; snip it out
                        (if (is cdr.it lastvalue)
                          (= lastvalue it))
                        (= cdr.it cddr.it)
                        ; reinsert in most-recently-called position
                        (insert args-and-result)
                        args-and-result.1)))
          )
    (fn args
      (or (lookup args)
          (ret result (apply f args)
            (insert (list args result)))))))

(mac def-lru (name params capacity . body)
  `(do (warn-if-bound ,name)
       (document def-lru ,name ,params
                   ,@(let newdoc (string "Caches last " capacity " unique calls.")
                       (if (is (type car.body) 'string)
                         (cons (+ car.body "\n" newdoc) cdr.body)
                         (cons newdoc body))))
       (assign ,name (lru ,capacity (fn ,params ,@body)))))
