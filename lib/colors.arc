
(def cat args
  (apply string "" args))

(def scat args
  (sym:cat args))


(= term*
   (obj colors (obj
                 black       0
                 red         1
                 green       2
                 yellow      3
                 blue        4
                 magenta     5
                 cyan        6
                 white       7)
        attrs  (obj
                 off         0 
                 reset       0
                 rst         0
                 bold        1  nobold      22
                 bld         1  nobld       22
                 ul          4  noul        24
                 underline   4  nounderline 24
                 blink       5  noblink     25
                 black      30  fgblack     30 bgblack    40
                 blk        30  fgblk       30 bgblk      40
                 red        31  fgred       31 bgred      41
                 green      32  fggreen     32 bggreen    42
                 grn        32  fggrn       32 bggrn      42
                 yellow     33  fgyellow    33 bgyellow   43
                 ylw        33  fgylw       33 bgylw      43
                 blue       34  fgblue      34 bgblue     44
                 blu        34  fgblu       34 bgblu      44
                 magenta    35  fgmagenta   35 bgmagenta  45
                 mag        35  fgmag       35 bgmag      45
                 cyan       36  fgcyan      36 bgcyan     46
                 cyn        36  fgcyn       36 bgcyn      46
                 white      37  fgwhite     37 bgwhite    47
                 wht        37  fgwht       37 bgwht      47)))

(def denil (xs)
  (keep ~no xs))

(def windows () nil)

(def prcode (codes (o sep #\;))
  (unless (windows)
    (let xs (flat:list codes)
      (pr (cat "\033[" (apply cat (intersperse sep denil.xs)) "m")))))


(def lerp (a b vt)
  ;(w/infix vt `* (b `- a) `+ a))
  (+ a (* vt (- b a))))

(def rgb16 (r g b (o bg nil))
  (+ 16 (* 36 (trunc:lerp 0 6 (/ r 256.0)))
        (*  6 (trunc:lerp 0 6 (/ g 256.0)))
              (trunc:lerp 0 6 (/ b 256.0))))

(def termval (val)
  (term*!attrs val))

(def termfx fxs
  (when (some [in (type _) 'int 'num] fxs)
    (= fxs (list fxs)))
  (each val fxs
    (if (acons val)
        (if (is len.val 3)  (apply termrgb val)
            (caris val 'fg) (apply termrgb (cdr val))
            (caris val 'bg) (apply termrgb (+ (cdr val) '(t)))
                            (err "termfx: unknown spec" val))
        (prcode (term*!attrs val)))))

(def termrgb (r g b (o bg))
  (prcode (list (if bg 48 38) 5 (rgb16 r g b))))

(def prcol (col s)
  (pr (cat "\033[1;" (string (+ 40 (term*!colors col))) "m"))
  (pr s)
  (pr "\033[1;0m")
  s)

(def prfx (spec . vals)
  (after
    (do (apply termfx (flat:list spec))
        (apply pr vals))
    (termfx nil)))

(def prnfx args
  (apply prfx args)
  (prn))

(def mkfx args
  (tostring:apply prfx args))

(mac w/fx (spec . vals)
  (let f (afn (xs)
           (when xs
             (let x (car xs)
               (cons 
                 (if (and (acons x) (caris x 'unquote))
                    (cadr x)
                    (list 'quote x))
                 (self (cdr xs))))))
   `(tostring (prfx ',spec (cat ,@(intersperse #\space (f vals)))))))


(mac w/colors (var . body)
  `(each ,var '(blk red grn ylw blu mag cyn wht)
     (let ,(scat 'bg var) (scat 'bg ,var)
       ,@body)))

(mac w/attrs (var . body)
  `(each ,var `(() (bold) (ul) (bold ul) (bold ul blink))
     ,@body))
  
(def prstyles (x)
  (w/attrs attrs
    (prfx attrs x)
    (prn " ")
    (w/colors col
      (prfx `(  ,col ,@attrs) x) (pr " ")
      (prfx `(,bgcol ,@attrs) x) (pr " ")
      (w/colors fg
        (prfx `(,bgcol ,fg ,@attrs) x) (pr " "))
      (prn))
    (prn)))
      

(def makeprs (colors)
  (accum a
    (each col colors
      (a `(def ,(scat 'pr  col) (s) (prcol ',col s)))
      (a `(def ,(scat 'prn col) (s) (prcol ',col s) (prn)))
      (a `(def ,(scat 'mk  col) (s) (tostring:prcol ',col s))))))

(mac evaldo (code)
  `(eval `(do ,@(,@code))))

(evaldo:makeprs (keys term*!colors))

(def highlight (val str)
  (replace val (mkred val) str))

(mac w/highlight (val . body)
  `(let str (tostring (do ,@body))
     (void:pr (highlight ,val str))))

(def replace (old new str (o start 0))
  (iflet i (posmatch old str start)
         (let s (+ (cut str 0 i)
                   new
                   (cut str (+ i (len old))))
           (replace old new s (+ i (len new))))
         str))



