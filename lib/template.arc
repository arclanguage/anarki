; originally from http://awwx.ws/template5.arc

(implicit guillemet pr)

; turn adjacent characters into a string

(def coalesce-chars (lst)
  (xloop (lst lst a nil)
    (if (no lst)
         (rev a)
         (let (chars rest) (span [is (type _) 'char] lst)
           (if chars
                (next rest (cons (coerce chars 'string) a))
                (next cdr.lst (cons car.lst a)))))))

(def slurp-template-field (closing port)
  (read:string:accum a
    (whiler c (readc port) [or (no _) (is _ closing)]
      (a c))))

(def slurp-template (port (o accum))
  (cons 'template
        (coalesce-chars:accum a
          (whiler c (readc port) [or (no _) (is _ #\”)]
            (a (if (is c #\«)
                    (list 'guillemet (slurp-template-field #\» port))
                   (is c #\⌊)
                    (list 'pr (slurp-template-field #\⌋ port))
                    c))))))

(extend-readtable #\“ slurp-template)

(mac template xs
  `(do ,@(map [if (isa _ 'string)
                   `(pr ,_)
                   _]
              xs)))
