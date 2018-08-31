(require 'lib/html.arc)

; http://daringfireball.net/projects/markdown/syntax

(def md-from-form (str (o nolinks))
  (markdown (trim (rem #\return (esc-tags str)) 'end) 60 nolinks))

(def markdown (s (o maxurl) (o nolinks))
  (let ital nil
    (tostring
      (forlen i s
        (iflet (newi spaces) (indented-code s i (if (is i 0) 2 0))
          (do (pr  "<p><pre><code>")
            (let cb (code-block s (- newi spaces 1))
              (pr cb)
              (= i (+ (- newi spaces 1) (len cb))))
            (pr "</code></pre>"))
          (iflet newi (parabreak s i (if (is i 0) 1 0))
             (do (unless (is i 0) (pr "<p>"))
                 (= i (- newi 1)))
            (and (is (s i) #\*)
                 (or ital
                     (atend i s)
                     (and (~whitec (s (+ i 1)))
                          (pos #\* s (+ i 1)))))
             (do (pr (if ital "</i>" "<i>"))
                 (= ital (no ital)))
            (and (no nolinks)
                 (or (headmatch "http://" s i)
                     (headmatch "https://" s i)))
             (withs (n   (urlend s i)
                     url (clean-url (cut s i n)))
               (tag (a href url rel 'nofollow)
                 (pr (if (no maxurl) url (ellipsize url maxurl))))
               (= i (- n 1)))
             (writec (s i))))))))

(def indented-code (s i (o newlines 0) (o spaces 0))
  (let c (s i)
    (if (nonwhite c)
         (if (and (> newlines 1) (> spaces 1))
           (list i spaces)
           nil)
        (atend i s)
         nil
        (is c #\newline)
         (indented-code s (+ i 1) (+ newlines 1) 0)
         (indented-code s (+ i 1) newlines       (+ spaces 1)))))

; If i is start a paragraph break, returns index of start of next para.

(def parabreak (s i (o newlines 0))
  (let c (s i)
    (if (or (nonwhite c) (atend i s))
      (if (> newlines 1) i nil)
      (parabreak s (+ i 1) (+ newlines (if (is c #\newline) 1 0))))))

; Returns the indices of the next paragraph break in s, if any.

(def next-parabreak (s i)
  (unless (atend i s)
    (aif (parabreak s i)
      (list i it)
      (next-parabreak s (+ i 1)))))

(def paras (s (o i 0))
  (if (atend i s)
    nil
    (iflet (endthis startnext) (next-parabreak s i)
      (cons (cut s i endthis)
            (paras s startnext))
      (list (trim (cut s i) 'end)))))


; Returns the index of the first char not part of the url beginning
; at i, or len of string if url goes all the way to the end.

; Note that > immediately after a url (http://foo.com>) will cause
; an odd result, because the > gets escaped to something beginning
; with &, which is treated as part of the url.  Perhaps the answer
; is just to esc-tags after markdown instead of before.

; Treats a delimiter as part of a url if it is (a) an open delimiter
; not followed by whitespace or eos, or (b) a close delimiter
; balancing a previous open delimiter.

(def urlend (s i (o indelim))
  (let c s.i
    (if (atend i s)
      (if (closedelim c)
            (if indelim (+ i 1) i)
          (or punc.c whitec.c opendelim.c)
            i
          'else
            (+ i 1))
      (let nextc (s (+ i 1))
        (if (or whitec.c
                (and punc.c whitec.nextc)
                (and (or whitec.nextc punc.nextc)
                     (or opendelim.c
                         (and closedelim.c no.indelim))))
          i
          (urlend s (+ i 1) (or (opendelim c)
                                (and indelim (~closedelim c)))))))))

(def opendelim (c)  (in c #\< #\( #\[ #\{))

(def closedelim (c) (in c #\> #\) #\] #\}))


(def code-block (s i)
  (tostring
    (until (let left (- (len s) i 1)
             (or (is left 0)
                 (and (> left 2)
                      (is (s (+ i 1)) #\newline)
                      (nonwhite (s (+ i 2))))))
     (writec (s (++ i))))))

(def unmarkdown (s)
  (tostring
    (forlen i s
      (if (headmatch "<p>" s i)
           (do (++ i 2)
               (unless (is i 2) (pr "\n\n")))
          (headmatch "<i>" s i)
           (do (++ i 2) (pr #\*))
          (headmatch "</i>" s i)
           (do (++ i 3) (pr #\*))
          (headmatch "<a href=" s i)
           (let endurl (posmatch [in _ #\> #\space] s (+ i 9))
             (if endurl
               (do (pr (cut s (+ i 9) (- endurl 1)))
                   (= i (aif (posmatch "</a>" s endurl)
                             (+ it 3)
                             endurl)))
               (writec (s i))))
          (headmatch "<pre><code>" s i)
           (awhen (posmatch "</code></pre>" s (+ i 12))
             (pr (cut s (+ i 11) it))
             (= i (+ it 12)))
          (writec (s i))))))
