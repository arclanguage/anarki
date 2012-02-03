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
  (let x (table)
    (each (k v) (if (acons tem) tem (templates* tem))
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (w/infile i file (drain:temread tem i)))

(def temstore (tem val file)
  (writefile (temlist tem val) file))

(def temread (tem (o str (stdin)))
  (let x (read str 'eof)
    (if (~is 'eof x)
      (listtem tem x))))

(def temwrite (tem val (o o (stdout)))
  (write (temlist tem val) o))

; like coerce, but requires a template and ignores unknown fields
(def listtem (tem raw)
  (with (x (inst tem) fields (if (acons tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

; like tablist, but include nil fields
(def temlist (tem val)
  (ret fields (coerce val 'cons)
    (each (k v) (if acons.tem
                  tem
                  templates*.tem)
      (if (~assoc k fields)
        (push (list k nil) fields)))))
