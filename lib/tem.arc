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
                       (coerce pair.args 'table)
                       (memtable (map car (keep no:cadr pair.args))))))

(extend sref (tem v k) (isa tem 'tem)
  (sref rep.tem.1 v k)
  (if v
    (wipe rep.tem.2.k)
    (set rep.tem.2.k)))

(defcall tem (tem k)
  (or rep.tem.1.k
      (if (no rep.tem.2.k)
        ((alref (templates* rep.tem.0) k)))))

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
  (apply inst tem (apply + fields)))

; like tablist, but include nil fields
(def temlist (tem val)
  (ret fields (coerce rep.val.1 'cons)
    (each (k v) (if acons.tem
                  tem
                  templates*.tem)
      (if (~assoc k fields)
        (push (list k nil) fields)))))
