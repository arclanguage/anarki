(def unbound (x)
  (let l nil
    (ontree (fn (_) (if (and (isa _ 'sym) (~bound _)) (push _ l))) x)
    (sort < (dedup l))))

(mac make-br-fn (body)
     `(fn ,(unbound body) ,body))
