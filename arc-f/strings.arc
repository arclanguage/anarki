; Matching.  Spun off 29 Jul 06.

; arc> (tostring (writec (coerce 133 'char)))
;
;> (define ss (open-output-string))
;> (write-char (integer->char 133) ss)
;> (get-output-string ss)
;"\u0085"

(in-package strings)
(using <arc>v3)
(using <arc>v3-packages)
(using <arc>v3-testify)

(interface v1-parsing
  tokens)
(interface v1-url
  urldecode urlencode)
(interface v1-match
  litmatch posmatch endmatch headmatch
  begins
  findsubseq)
(interface v1-subst
  subst multisubst)
(interface v1-util
  blank trim num plural)
(interface v1-re
  re re-match re-pos re-subst)
(interface v1
  <strings>v1-parsing
  <strings>v1-url
  <strings>v1-match
  <strings>v1-subst
  <strings>v1-util
  <strings>v1-re)

(def tokens (s (o sep whitec))
  (let test (testify sep)
    (let rec (afn (cs toks tok)
               (if (no cs)         (consif tok toks)
                   (test (car cs)) (self (cdr cs) (consif tok toks) nil)
                                   (self (cdr cs) toks (cons (car cs) tok))))
    (rev (map [coerce _ 'string]
              (map rev (rec (coerce s 'cons) nil nil)))))))

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
                (let code (coerce (cut s (+ i 1) (+ i 3))
                                  'int 16)
                  (writeb code)))
              (++ i 2))
          (writec c)))))

(def urlencode (s)
  " Encodes the string `s' using application/x-www-form-urlencoded
    encodation. "
  (tostring:let to-hex [coerce _ 'string 16]
   (forlen i s
     (let c (s i)
       (if (is c #\space)
             (writec #\+)
           ((andf ~alphadig [no (in _ #\_ #\.)]) c)
             (withs (code (coerce c 'int)
                     bytes
                     (if
                       (< code 128)
                         (list (to-hex code))
                       (< code 2048)
                         (list (to-hex (+ 192 (quotient code 64)))
                               (to-hex (+ 128 (mod code 64))))
                       (< code 65536)
                         (w/collect:givens
                             _    (collect (to-hex (+ 224 (quotient code 4096))))
                             code (mod code 4096)
                             _    (collect (to-hex (+ 128 (quotient code 64))))
                             code (mod code 64)
                           (collect (to-hex (+ 128 code))))
                         (w/collect:givens
                             _    (collect (to-hex (+ 240 (quotient code 262144))))
                             code (mod code 262144)
                             _    (collect (to-hex (+ 128 (quotient code 4096))))
                             code (mod code 4096)
                             _    (collect (to-hex (+ 128 (quotient code 64))))
                             code (mod code 64)
                           (collect (to-hex (+ 128 code))))))
               (each byte bytes
                 (writec #\%)
                 (if (< (len byte) 2)
                     (writec #\0))
                 (pr byte)))
           (writec c))))))

(mac litmatch (pat string (o start 0))
  (w/uniq (gstring gstart)
    `(with (,gstring ,string ,gstart ,start)
       (unless (> (+ ,gstart ,(len pat)) (len ,gstring))
         (and ,@(w/collect:forlen i pat
                    (collect `(is ,(pat i) (,gstring (+ ,gstart ,i))))))))))

; litmatch would be cleaner if map worked for string and integer args:

;             ,@(map (fn (n c) 
;                      `(is ,c (,gstring (+ ,gstart ,n))))
;                    (len pat)
;                    pat)

(mac endmatch (pat string)
  (w/uniq (gstring glen)
    `(withs (,gstring ,string ,glen (len ,gstring))
       (unless (> ,(len pat) (len ,gstring))
         (and ,@(w/collect:forlen i pat
                  (collect `(is ,(pat (- (len pat) 1 i)) 
                                (,gstring (- ,glen 1 ,i))))))))))

(def posmatch (pat seq (o start 0))
  (breakable
    (if (isa pat 'fn)
        (for i start (- (len seq) 1)
             (when (pat (seq i)) (break i)))
        (for i start (- (len seq) (len pat))
             (when (headmatch pat seq i) (break i))))
    nil))

(def headmatch (pat seq (o start 0))
  ; warning: can fail if pat is longer than seq!
  ; preferentially use 'begins below!
  ; (sigh.  PG's naming conventions are bunk)
  (let p (len pat) 
    ((afn (i j)
       (or (is i p) 
           (and (is (pat i) (seq j))
                (self (+ i 1) (+ j 1)))))
     0 start)))

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

(= -front (unpkg 'front))
(= -both  (unpkg 'both))
(= -end   (unpkg 'end))
(def trim (s where (o test whitec))
  (withs (f   (testify test)
              ; TODO: fix this when Arc4F properly exports
              ; <arc>pos in the <arc>v4 interface
           p1 (<arc>pos ~f s))
    (if p1
        (cut s 
             (if (in (unpkg where) -front -both) p1 0)
             (if (in (unpkg where) -end -both)
               (let i (- (len s) 1)
                 (while (and (> i p1) (f (s i)))
                        (-- i))
                 (+ i 1))
               (len s)))
        "")))

(def num (m (o digits 2) (o trail-zeros nil) (o init-zero nil))
  (let comma
       (fn (i)
         (tostring
           (map [apply pr (rev _)]
                (rev (intersperse '(#\,)
                                  (tuples (rev (coerce (string i) 'cons))
                                          3))))))
    (if (< digits 1)
         (comma (roundup m))
        (exact m)
         (string (comma m)
                 (when (and trail-zeros (> digits 0))
                   (string "." (newstring digits #\0))))
         (withs (d (expt 10 digits)
                 n (/ (roundup (* m d)) d)
                 i (trunc n))
           (+ (if (is i 0) (if init-zero "0" "") (comma i))
              (withs (rest   (string (abs (trunc (- (* n d) (* i d)))))
                      padded (+ (newstring (- digits (len rest)) #\0)
                                rest)
                      final  (if trail-zeros
                                 padded
                                 (trim padded 'end [is _ #\0])))
                (string (unless (empty final) ".")
                        final)))))))

; English

(def plural (n str)
  (if (or (is n 1) (single n))
      str
      (string str "s")))

; Import Scheme's regular expressions
(using <arc>v3-on-mzscheme)
(= re ($ regexp))
(= re-match (fn (_1 _2) (no (no (($ regexp-match) _1 _2)))))
(= re-pos ($ regexp-match-positions))
(= re-subst ($ regexp-replace))


; http://www.eki.ee/letter/chardata.cgi?HTML4=1
; http://jrgraphix.net/research/unicode_blocks.php?block=1
; http://home.tiscali.nl/t876506/utf8tbl.html
; http://www.fileformat.info/info/unicode/block/latin_supplement/utf8test.htm
; http://en.wikipedia.org/wiki/Utf-8
; http://unicode.org/charts/charindex2.html
