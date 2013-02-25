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

; (tagged 'tem (tem-type fields nils))
(def inst (tem-type . args)
  (annotate 'tem (list tem-type
                       (w/table x
                         (each (k v) (if acons.tem-type tem-type templates*.tem-type)
                           (unless no.v
                             (= x.k (v))))
                         (each (k v) pair.args
                           (= x.k v))))))

(extend sref (tem v k) (isa tem 'tem)
  (sref rep.tem.1 v k))

(defcall tem (tem k)
  rep.tem.1.k)

(defmethod iso(a b) tem
  (and (isa a 'tem)
       (isa b 'tem)
       (iso rep.a rep.b)))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (w/infile i file (drain:temread tem i)))

(def temstore (tem val file)
  (writefile (temlist tem val) file))

(def temread (tem (o str (stdin)))
  (reading fields str
    (listtem tem fields)))

(def temwrite (tem val (o o (stdout)))
  (write (temlist tem val) o))

; coerce alist to a specific template
(def listtem (tem fields)
  (apply inst tem (if fields
                    (apply + fields))))

; like tablist, but include explicitly-set nil fields
(def temlist (tem val)
  (coerce rep.val.1 'cons))
