
(require "lib/settable-fn.arc")

(def bidir-table ()
  " Creates a bidirectional table.  Works like a normal
    table but returns keys when queried with values.
    See also [[table]] "
  (with (k-to-v (table)
         v-to-k (table))
    (add-attachments
      '= (afn (v k)
           ; determine if delete or assign
           (aif
             ; insert new pair
             v
               (do
                 ; delete any existing pairs first
                 (self nil k)
                 (self nil v)
                 ; add it
                 ;  no point assigning this to v-to-k
                 ;  if v==k, since k-to-v will return
                 ;  that mapping first
                 (if (isnt k v) (= (v-to-k v) k))
                 (= (k-to-v k) v))
             ; deleted k
             (k-to-v k)
               (= (v-to-k it) nil
                  (k-to-v k) nil)
             ; deleted v
             (v-to-k k)
               (= (k-to-v it) nil
                  (v-to-k k) nil)))
      ; Only return items which were assigned as
      ; keys, so that 'ontable doesn't go over
      ; pairings twice.
      'keys (fn () (keys k-to-v))
      (annotate 'table
        (fn (k) (or (k-to-v k) (v-to-k k)))))))

