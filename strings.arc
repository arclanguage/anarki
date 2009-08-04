; Matching.  Spun off 29 Jul 06.

; arc> (tostring (writec (coerce 133 'char)))
;
;> (define ss (open-output-string))
;> (write-char (integer->char 133) ss)
;> (get-output-string ss)
;"\u0085"

(def tokens (s (o sep whitec))
  (let test (testify sep)
    (let rec (afn (cs toks tok)
               (if (no cs)         (consif tok toks)
                   (test (car cs)) (self (cdr cs) (consif tok toks) nil)
                                   (self (cdr cs) toks (cons (car cs) tok))))
      (rev (map [coerce _ 'string]
                (map rev (rec (coerce s 'cons) nil nil)))))))

; names of cut, split, halve not optimal

(def halve (s (o sep whitec))
  (let test (testify sep)
    (let rec (afn (cs tok)
               (if (no cs)         (list (rev tok))
                   (test (car cs)) (list cs (rev tok))
                                   (self (cdr cs) (cons (car cs) tok))))
      (rev (map [coerce _ 'string]
                (rec (coerce s 'cons) nil))))))

; maybe promote to arc.arc, but if so include a list clause

(def positions (test seq)
  (accum a
    (let f (testify test)
      (forlen i seq
        (if (f (seq i)) (a i))))))

(def lines (s)
  (accum a
    ((afn ((p . ps))
       (if ps
           (do (a (rem #\return (cut s (+ p 1) (car ps))))
               (self ps))
           (a (cut s (+ p 1)))))
     (cons -1 (positions #\newline s)))))

(def slices (s test)
  (accum a
    ((afn ((p . ps))
       (if ps
           (do (a (cut s (+ p 1) (car ps)))
               (self ps))
           (a (cut s (+ p 1)))))
     (cons -1 (positions test s)))))

; > (require (lib "uri-codec.ss" "net"))
;> (form-urlencoded-decode "x%ce%bbx")
;"xλx"

; first byte: 0-7F, 1 char; c2-df 2; e0-ef 3, f0-f4 4. 

; Fixed for utf8 by pc.

(def urldecode (s)
 (tostring
  (forlen i s
    (caselet c (s i)
      #\+ (writec #\space)
      #\% (do (when (> (- (len s) i) 2)
                (writeb (int (cut s (+ i 1) (+ i 3)) 16)))
              (++ i 2))
          (writec c)))))

(def urlencode (s)
  (tostring 
    (each c s 
      (writec #\%)
      (let i (int c)
        (if (< i 16) (writec #\0))
        (pr (coerce i 'string 16))))))

(mac litmatch (pat string (o start 0))
  (w/uniq (gstring gstart)
    `(with (,gstring ,string ,gstart ,start)
       (unless (> (+ ,gstart ,(len pat)) (len ,gstring))
         (and ,@(let acc nil
                  (forlen i pat
                    (push `(is ,(pat i) (,gstring (+ ,gstart ,i)))
                           acc))
                  (rev acc)))))))

; litmatch would be cleaner if map worked for string and integer args:

;             ,@(map (fn (n c)  
;                      `(is ,c (,gstring (+ ,gstart ,n))))
;                    (len pat)
;                    pat)

(mac endmatch (pat string)
  (w/uniq (gstring glen)
    `(withs (,gstring ,string ,glen (len ,gstring))
       (unless (> ,(len pat) (len ,gstring))
         (and ,@(let acc nil
                  (forlen i pat
                    (push `(is ,(pat (- (len pat) 1 i)) 
                               (,gstring (- ,glen 1 ,i)))
                           acc))
                  (rev acc)))))))

(def posmatch (pat seq (o start 0))
  (catch
    (if (isa pat 'fn)
        (for i start (- (len seq) 1)
          (when (pat (seq i)) (throw i)))
        (for i start (- (len seq) (len pat))
          (when (headmatch pat seq i) (throw i))))
    nil))

(def headmatch (pat seq (o start 0))
  (let p (len pat) 
    ((afn (i)      
       (or (is i p) 
           (and (is (pat i) (seq (+ i start)))
                (self (+ i 1)))))
     0)))

(def begins (seq pat (o start 0))
  (unless (len> pat (- (len seq) start))
    (headmatch pat seq start)))

(def subst (new old seq)
  (let boundary (+ (- (len seq) (len old)) 1)
    (tostring 
      (forlen i seq
        (if (and (< i boundary) (headmatch old seq i))
            (do (++ i (- (len old) 1))
                (pr new))
            (pr (seq i)))))))

(def multisubst (pairs seq)
  (tostring 
    (forlen i seq
      (iflet (old new) (find [begins seq (car _) i] pairs)
        (do (++ i (- (len old) 1))
            (pr new))
        (pr (seq i))))))

; not a good name

(def findsubseq (pat seq (o start 0))
  (if (< (- (len seq) start) (len pat))
       nil
      (if (headmatch pat seq start)
          start
          (findsubseq pat seq (+ start 1)))))

(def blank (s) (~find ~whitec s))

(def nonblank (s) (unless (blank s) s))

(def trim (s (o where 'both) (o test whitec))
  (withs (f   (testify test)
           p1 (pos ~f s))
    (if p1
        (cut s 
             (if (in where 'front 'both) p1 0)
             (when (in where 'end 'both)
               (let i (- (len s) 1)
                 (while (and (> i p1) (f (s i)))
                   (-- i))
                 (+ i 1))))
        "")))

(def num (n (o digits 2) (o trail-zeros nil) (o init-zero nil))
  (withs (comma
          (fn (i)
            (tostring
              (map [apply pr (rev _)]
                   (rev (intersperse '(#\,)
                                     (tuples (rev (coerce (string i) 'cons))
                                             3))))))
          abrep
          (let a (abs n)
            (if (< digits 1)
                 (comma (roundup a))
                (exact a)
                 (string (comma a)
                         (when (and trail-zeros (> digits 0))
                           (string "." (newstring digits #\0))))
                 (withs (d (expt 10 digits)
                         m (/ (roundup (* a d)) d)
                         i (trunc m)
                         r (abs (trunc (- (* m d) (* i d)))))
                   (+ (if (is i 0) 
                          (if (or init-zero (is r 0)) "0" "") 
                          (comma i))
                      (withs (rest   (string r)
                              padded (+ (newstring (- digits (len rest)) #\0)
                                        rest)
                              final  (if trail-zeros
                                         padded
                                         (trim padded 'end [is _ #\0])))
                        (string (unless (empty final) ".")
                                final)))))))
    (if (and (< n 0) (find [and (digit _) (isnt _ #\0)] abrep))
        (+ "-" abrep)
        abrep)))
        

; English

(def pluralize (n str)
  (if (or (is n 1) (single n))
      str
      (string str "s")))

(def plural (n x)
  (string n #\  (pluralize n x)))


; http://www.eki.ee/letter/chardata.cgi?HTML4=1
; http://jrgraphix.net/research/unicode_blocks.php?block=1
; http://home.tiscali.nl/t876506/utf8tbl.html
; http://www.fileformat.info/info/unicode/block/latin_supplement/utf8test.htm
; http://en.wikipedia.org/wiki/Utf-8
; http://unicode.org/charts/charindex2.html
