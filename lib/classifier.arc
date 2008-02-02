; A simple bayesian classifier ported from python (from "Collective Intelligence")

; helper

(mac fc (x) `(car ,x))
(mac cc (x) `(cadr ,x))
(mac mkcls ((o tresholds '(table))) `(list (table) (table) ,tresholds))

(mac tresh (x cat) (w/uniq q `(let ,q ((cadr:cdr ,x) ,cat) (or ,q 1.0))))
(mac =tresh (x cat v) `(do (= ((cadr:cdr ,x) ,cat) ,v) ,x))

(mac =default (table key de)
  (w/uniq (t k d)
  `(with (,t ,table ,k ,key ,d ,de)
    (if (,t ,k)
      ,t
      (do (= (,t ,k) ,d)
          ,t)))))

(def incf (cl f cat)
  (do (=default (fc cl) f (table))
      (=default ((fc cl) f) cat 0)
    (let fctab ((fc cl) f)
      (= (fctab cat) (+ 1 (fctab cat))))
      cl))

(def incc (cl cat)
  (do (=default (cc cl) cat 0) ;(prn (cc cl))
      (= ((cc cl) cat) (+ 1 ((cc cl) cat)))
      cl))

(def fcount (cl f cat)
  (if (and ((fc cl) f) (((fc cl) f) cat))
  (((fc cl) f) cat)
  0))

(def catcount (cl cat)
  (or ((cc cl) cat) 0))

(def totalcount (cl)
  (apply + (vals (cc cl))))

(def categories (cl)
  (keys (cc cl)))

; use dedup on words
(def train (cl words cat)
  (do (map [incf cl _ cat] words)
      (incc cl cat)
      cl))

; probability & classifier stuff

; P( word | class )
(def fprob (cl f cat)
  (if (is 0 (catcount cl cat))
      0
      (/ (fcount cl f cat) (catcount cl cat))))

; w = weight of ap
; ap = assumed probability
; pf = probability func
(def weighted-prob (cl f cat pf w (o ap 0.5))
  (with (basicprob (pf cl f cat)
         totals (apply + (map [fcount cl f _] (categories cl))))
     (/ (+ (* w ap) (* totals basicprob))
        (+ w totals))))
                           

; wcount = take only wcount highest scoring words of that category
(def doc-prob (cl cat words (o wcount 15))
  (apply * (bestn wcount > (map [weighted-prob cl _ cat fprob 1 0.5] (dedup words)))))

; P( Category | Document ) = P( Document | Category ) * P ( Category ) / P ( Document )
(def naivebayes (cl cat words)
  (with (catprob (/ (catcount cl cat) (totalcount cl))
         docprob (doc-prob cl cat words))
     (* docprob catprob)))

; use a dedup on the words list
; returns 'unknown if the best probability isn't above  (* treshold other_categories)
(def classify (cl words)
  (let ((best x). rest) (sort (fn ((c1 x) (c2 y)) (> x y)) 
                              (map (fn (cat) (list cat (naivebayes cl cat words))) (categories cl)))
    (if (trues (fn ((c y)) (> (* y (tresh cl best)) x)) rest)
        'unknown
        best)))

(def sampletrain (cl)
  (do
  (train cl '(Nobody owns the water) 'good)
  (train cl '(the quick rabbit jumps fences) 'good)
  (train cl '(buy pharmaceuticals now) 'bad)
  (train cl '(make quick money at the online casino) 'bad)
  (train cl '(the quick brown fox jumps) 'good)
  cl))

(do (= cl2 (sampletrain (mkcls))) "training...")
(classify  cl2 '(quick rabbit))
(classify  cl2 '(quick money))
(do (=tresh cl2 'bad 3.0) "changing treshold: use to remove possible false positives.")
(classify  cl2 '(quick money))
(do (repeat 10 (sampletrain cl2)) "training...")
(classify cl2 '(quick money))
