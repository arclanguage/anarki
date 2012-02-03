(= templates* (table))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name)
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name)
      (union (fn (x y) (is (car x) (car y)))
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (pair fields)))
             (templates* ',name))))

(def inst (tem . args)
  (annotate 'tem (list tem (coerce pair.args 'table))))

; coerce alist to a specific template
(def listtem (tem fields)
  (apply inst tem (apply + fields)))

(extend sref (tem v k) (isa tem 'tem)
  (sref rep.tem.1 v k))

(defcall tem (tem k)
  (or rep.tem.1.k
      (let f (alref (templates* rep.tem.0) k)
        (f))))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (w/infile i file (drain:temread tem i)))

(def temstore (tem val file)
  (let fields (coerce rep.val 'list)
    (each (k v) (if acons.tem
                  tem
                  templates*.tem)
      (if (~assoc k fields)
        (push (list k nil) fields)))
    (writefile fields file)))

(def temread (tem (o str (stdin)))
  (let fields (read str 'eof)
    (if (~is 'eof fields)
      (listtem tem fields))))
