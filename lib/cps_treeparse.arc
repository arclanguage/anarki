; cps_treeparse.arc
; by AmkG
; A continuation-passing variant of parser combinations
; Experimental; used for reference comparison with
; monadic treeparse.arc

; unlike treeparse.arc, this has been designed from scratch
; for speed.  Unfortunately for this library, it's (1)
; continuation-passing style, which is hard on the eyes,
; and (2) the guy who wrote this doesn't know how to
; optimize, not really.

; Limitations compared to monadic treeparse.arc:
; 1.  Does not support 'sem.  I would say that about 30% of
; the speed difference occurs here ^^
; 2.  You can't call parsers directly - you *must* use
; 'parse to parse
; 3.  Some weird edge cases you probably shouldn't do -
; such as (seq) without any arguments - will act differently

(mac delay-parser (p)
  (w/uniq (remaining pass fail)
    `(fn (,remaining ,pass ,fail)
       (,p ,remaining ,pass ,fail))))

(mac assign-parser (var p)
  (w/uniq (li pass fail parsed parsedtl remaining)
    `(fn (,li ,pass ,fail)
       (,p ,li
           (fn (,parsed ,parsedtl ,remaining)
               (= ,var (copy ,parsed))
               (,pass ,parsed ,parsedtl ,remaining))
           ,fail))))

(def parse (parser remaining)
  (if
    (isa parser 'fn)
      (point k
        (parser remaining
          ; pass
          (fn (v _ remaining) (k (list v remaining)))
          ; fail
          (fn () (k nil))))
    (acons parser)
      (parse-list parser remaining)
      (parse (lit parser) remaining)))

(def lit (a)
  (fn (remaining pass fail)
    (let cr (car remaining)
      (if (and (acons remaining) (iso a cr))
          (let v (cons cr nil)
            (pass v v (cdr remaining)))
          (fail)))))

(def litify (a)
  (if (isa a 'fn) a (lit a)))

(def nil-lit (a)
  (fn (remaining pass fail)
    (if (and (acons remaining) (iso a (car remaining)))
        (pass nil nil (cdr remaining))
        (fail))))

(def nil-litify (a)
  (if (isa a 'fn) a (nil-lit a)))

(def at-end (remaining pass fail)
  (if remaining (fail)
      (pass nil nil nil)))

(def seq parsers
  (seq-l parsers))

(def seq-l (parsers)
  (fn (remaining pass fail)
    ((seq-r) parsers remaining pass fail)))

(def seq-r ()
  (let (acc acctl) nil
    (afn (parsers remaining pass fail)
      (let parser (car parsers)
        (zap litify parser)
        (parser remaining
          (fn (parsed parsedtl remaining)
            (if acc
                (when parsed
                    (= (cdr acctl) parsed)
                    (= acctl parsedtl))
                (= acc parsed acctl parsedtl))
            (let parsers (cdr parsers)
              (if parsers
                  (self parsers remaining pass fail)
                  (pass acc acctl remaining))))
          fail)))))

(def nil-seq parsers
  (nil-seq-l parsers))

(def nil-seq-l (parsers)
  (let parser (car parsers)
    (zap nil-litify parser)
    (fn (remaining pass fail)
      (parser remaining
        (fn (_ __ remaining)
          (let parsers (cdr parsers)
            (if parsers
                ((nil-seq-l parsers) remaining pass fail))
                (pass nil nil remaining)))
        fail))))

(def many (parser)
  (zap litify parser)
  (fn (remaining pass _)
    ((many-r parser) remaining pass)))

(def many-r (parser)
  (let (acc acctl) nil
    (afn (remaining pass)
      (parser remaining
        (fn (parsed parsedtl remaining)
          (if acc
              (when parsed
                  (= (cdr acctl) parsed)
                  (= acctl parsedtl))
              (= acc parsed acctl parsedtl))
          (self remaining pass))
        (fn ()
          (pass acc acctl remaining))))))

(def many1 (parser)
  (seq parser (many parser)))

(def many2 (parser)
  (seq parser (many1 parser)))

(def nil-many (parser)
  (zap nil-litify parser)
  (fn (remaining pass _)
    ((nil-many-r parser) remaining pass)))

(def nil-many-r (parser)
  (afn (remaining pass)
    (parser remaining
      (fn (_ __ remaining)
        (self remaining pass))
      (fn ()
        (pass nil nil remaining)))))

(def nil-many1 (parser)
  (nil-seq parser (nil-many parser)))

(def nil-many2 (parser)
  (nil-seq parser (nil-many1 parser)))

(def alt parsers
  (alt-l parsers))

(def alt-l (parsers)
  (fn (remaining pass fail)
    ((alt-r remaining pass fail) parsers)))

(def alt-r (remaining pass fail)
  (afn (parsers)
    (if parsers
        (with (parser (car parsers)
               parsers (cdr parsers))
          (zap litify parser)
          (parser remaining
            pass
            (fn ()
              (self parsers))))
        (fail))))

(def nothing (remaining pass _)
  (pass nil nil remaining))

(def anything (remaining pass fail)
  (if (acons remaining)
      (let v (cons (car remaining) nil)
        (pass v v (cdr remaining)))
      (fail)))

(withs (lastcdr
        (afn (p) (aif (cdr p) (self it) p))
        new-pass
        (fn (fun pass)
          (fn (parsed _ remaining)
            (let v (fun parsed)
              (pass v (lastcdr v) remaining)))))
  (def filt (fun parser)
    (zap litify parser)
    (fn (remaining pass fail)
      (parser remaining
        (new-pass fun pass)
        fail))))

(let new-pass
     (fn (fun pass)
       (fn (parsed parsedtl remaining)
         ((afn (p)
            (when p
              (zap fun (car p))
              (self (cdr p))))
          parsed)
         (pass parsed parsedtl remaining)))
  (def filt-map (fun parser)
    (zap litify parser)
    (fn (remaining pass fail)
      (parser remaining
        (new-pass fun pass)
        fail))))

(def cant-see (parser)
  (zap litify parser)
  (fn (remaining pass fail)
    (parser remaining
      (fn (_ __ ___) (fail))
      (fn ()
        (pass nil nil remaining)))))

(def anything-but parsers
  (seq (cant-see (alt-l parsers)) anything))

(def maybe (parser)
  (alt parser nothing))

(def pred (test parser)
  (zap litify parser)
  (fn (remaining pass fail)
    (parser remaining
      (fn (parsed parsedtl remaining)
        (if (test parsed)
            (pass parsed parsedtl remaining)
            (fail)))
      fail)))

