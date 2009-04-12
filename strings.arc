; Matching.  Spun off 29 Jul 06.

; Arc> (tostring (writec (coerce 133 'char)))
;
;> (define ss (open-output-string))
;> (write-char (integer->char 133) ss)
;> (get-output-string ss)
;"\u0085"

(def tokens (s (o sep whitec))
  (let test (if (isa sep 'fn) sep (fn (c) (is c sep)))
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

(def urldecode (s)
  (tostring
    (forlen i s
      (caselet c (s i)
        #\+ (writec #\space)
        #\% (do (when (> (- (len s) i) 2)
                  (let code  (coerce (subseq s (+ i 1) (+ i 3))
                                     'int 16)
                    (if (> code 126)
                        (pr (latin1-hack code))
                        (writec (coerce code 'char)))))
                (++ i 2))
            (writec c)))))


; Converts utf8 chars between 128 and 255 to ascii char or string.
; Not able to convert every char; generates X if can't.  Probably not
; the ultimate solution.  The Right Thing would be to preserve these
; chars, instead of downgrading them to ascii.  To do that, would have
; to convert them to unicode.  E.g. ellipsis in unicode is #x2026,
; euro sign is #x20ac.  

; In Mzscheme: (display (integer->char #xE9))

; Then have to figure out how to print them back out, both in forms 
; and in pages.  Presumably do the reverse translation and &-escape them.

; For much of this range, unicode and utf8 agree.  233 is e-acute in
; both.  It's chars like 133 that are a problem.  So perhaps as a
; start try preserving e.g. 233.

; This would be faster if I made a macro that translated it into
; a hashtable or even string.

(def latin1-hack (i)
  (if (is i 128)      "EUR"    ; euros
      (is i 133)      "..."
      (<= 145 i 146)  #\'
      (<= 147 i 148)  #\"
      (is i 151)      "--"     ; long dash
      (is i 154)      #\S
      (is i 155)      #\>
      (is i 156)      "oe"
      (is i 158)      #\z
      (is i 159)      #\Y
      (is i 162)      #\c      ; cents
      (is i 163)      "GBP"    ; pounds
      (is i 165)      "JPY"    ; yen
      (is i 166)      #\|
      (is i 171)      "<<"
      (is i 187)      ">>"
      (<= 192 i 197)  #\A
      (is i 198)      "AE"
      (is i 199)      #\C
      (<= 200 i 203)  #\E
      (<= 204 i 207)  #\I
      (is i 209)      #\N
      (<= 210 i 214)  #\O
      (is i 215)      #\x
      (is i 216)      #\O
      (<= 217 i 220)  #\U
      (is i 221)      #\Y
      (is i 223)      "ss"
      (<= 224 i 229)  #\a
      (is i 230)      "ae"
      (is i 231)      #\c
      (<= 232 i 235)  #\e
      (<= 236 i 239)  #\i
      (is i 241)      #\n
      (<= 242 i 246)  #\o
      (is i 247)      #\/
      (is i 248)      #\o
      (<= 249 i 252)  #\u
      (in i 253 255)  #\y
      #\X
      ))

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
        (for i start (- (len seq) (- (len pat) 2))
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
  (unless (> (len pat) (- (len seq) start))
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

(def findsubseq (pat seq (o start 0))
  (if (< (- (len seq) start) (len pat))
       nil
      (if (headmatch pat seq start)
          start
          (findsubseq pat seq (+ start 1)))))

(def blank (s) (~find ~whitec s))

; should make it possible for test to be a literal as well

(def trim (s where (o test whitec))
  (let p1 (pos [no (test _)] s)
    (if p1
        (subseq s 
                (if (in where 'front 'both) p1 0)
                (when (in where 'end 'both)
                  (let i (- (len s) 1)
                    (while (and (> i p1) (test (s i)))
                      (-- i))
                    (+ i 1))))
        "")))

(def num (m (o digits 2) (o trail-zeros nil) (o init-zero nil))
  (let comma
       (fn (n)
         (tostring
           (map [apply pr (rev _)]
                (rev (intersperse '(#\,)
                                  (tuples (rev (coerce (string n) 'cons))
                                          3))))))
    (if (< digits 1)
         (comma (roundup m))
        (exact m)
         (string (comma m)
                 (when (and trail-zeros (> digits 0))
                   (string "." (newstring digits #\0))))
         (let n (truncate m)
           (string (if (is n 0) (if init-zero 0 "") (comma n))
                   "."
                   (withs (rest (string (abs (roundup
                                               (- (* m (expt 10 digits))
                                                  (* n (expt 10 digits))))))
                           v2 (string (newstring (- digits (len rest)) #\0)
                                      rest))
                     (if trail-zeros
                         v2
                         (trim v2 'end [is _ #\0]))))))))


; http://www.eki.ee/letter/chardata.cgi?HTML4=1
; http://jrgraphix.net/research/unicode_blocks.php?block=1
; http://home.tiscali.nl/t876506/utf8tbl.html
; http://www.fileformat.info/info/unicode/block/latin_supplement/utf8test.htm
; http://en.wikipedia.org/wiki/Utf-8
; http://unicode.org/charts/charindex2.html
