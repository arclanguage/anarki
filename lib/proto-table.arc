; proto-table.arc
; by AmkG
; creates a table which shadows another table-like structure
; (something like a prototype-object system)

(require "lib/settable-fn.arc")

(def proto-table (src . rest)
  (let nt (table)
    (fill-table nt rest)
    (add-attachments
      'keys
      (fn () (dedup:join (keys nt) (keys src)))
      '= (fn (v i) (= nt.i v))
      're-prototype [= src _]
      (annotate 'table
        (fn (i)
          (or nt.i src.i))))))

(def re-prototype (T nt)
  (let f (get-attachment 're-prototype T)
    (if f
        (f nt)
        (err "Cannot reattach prototype"))))

