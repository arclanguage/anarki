(seval '(require racket/date))

(= months* '("January" "February" "March" "April" "May" "June" "July"
             "August" "September" "October" "November" "December")
   days* '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))


(defmemo date-yearday ((y m d))
  (seval!date-year-day:seval!seconds->date:seval!find-seconds 0 0 0 d m y #f))

(defmemo date-weekday ((y m d))
  (seval!date-week-day:seval!seconds->date:seval!find-seconds 0 0 0 d m y #f))

(defmemo date-weekday-name (ymd (o short t))
  (let s (days* (date-weekday ymd))
    (if short (cut s 0 3) s)))

(defmemo month-name (m (o short t))
  (let s (months* (- m 1))
    (if short (cut s 0 3) s)))

(defmemo strftime (fmt (o ts (seconds)))
  ; TODO:
  ; By default, date pads numeric fields with zeroes.  The following
  ; optional flags may follow '%':
  ;
  ; -      (hyphen) do not pad the field
  ;
  ; _      (underscore) pad with spaces
  ;
  ; 0      (zero) pad with zeros
  ;
  ; ^      use upper case if possible
  ;
  ; #      use opposite case if possible
  ;
  ; After  any  flags  comes  an  optional field width, as a decimal
  ; number; then an optional modifier, which is either E to use the
  ; locale's alternate representations if available, or O to use the
  ; locale's alternate numeric symbols if available.
  (withs (secs (trunc ts)
          ns 0 ; todo
          (Y m d H M S) (rev:timedate secs)
          I (aand (mod H 12) (if (is it 0) 12 it))
          fmt (if (begins fmt "+") (cut fmt 1) fmt)
          n (len fmt))
    (apply string
      (accum out
        (forlen i fmt
          (if (~and (is (fmt i) #\%) (< i (- n 1)))
              (out (fmt i))
              (case (fmt (++ i))
; %%     a literal %
                #\% (out "%")
; %a     locale's abbreviated weekday name (e.g., Sun)
                #\a (out (date-weekday-name (list Y m d) t))
; %A     locale's full weekday name (e.g., Sunday)
                #\A (out (date-weekday-name (list Y m d) nil))
; %b     locale's abbreviated month name (e.g., Jan)
                #\b (out (month-name m t))
; %B     locale's full month name (e.g., January)
                #\B (out (month-name m nil))
; %c     locale's date and time (e.g., Thu Mar  3 23:05:25 2005)
                #\c (out (strftime "%a %b %e %H:%M:%S %Y" secs))
; %C     century; like %Y, except omit last two digits (e.g., 20)
                #\C (out (cut (str Y) 0 2))
; %d     day of month (e.g., 01)
                #\d (out (leftpad (str d) 2 "0"))
; %D     date; same as %m/%d/%y
                #\D (out (strftime "%m/%d/%y" secs))
; %e     day of month, space padded; same as %_d
                #\e (out (leftpad (str d) 2 " "))
; %F     full date; same as %Y-%m-%d
                #\F (out (strftime "%Y-%m-%d" secs))
; %g     last two digits of year of ISO week number (see %G)
; %G     year of ISO week number (see %V); normally useful only with %V
                ;#\g (err "todo")
                ;#\G (err "todo")
; %h     same as %b
                #\h (out (month-name m t))
; %H     hour (00..23)
                #\H (out (leftpad (str H) 2 "0"))
; %I     hour (01..12)
                #\I (out (leftpad (str I) 2 "0"))
; %j     day of year (001..366)
                #\j (out (leftpad (str (+ 1 (date-yearday (list Y m d)))) 3 "0"))
; %k     hour, space padded ( 0..23); same as %_H
                #\k (out (leftpad (str H) 2 " "))
; %l     hour, space padded ( 1..12); same as %_I
                #\l (out (leftpad (str I) 2 " "))
; %m     month (01..12)
                #\m (out (leftpad (str m) 2 "0"))
; %M     minute (00..59)
                #\M (out (leftpad (str M) 2 "0"))
; %n     a newline
                #\n (out "\n")
; %N     nanoseconds (000000000..999999999)
                #\N (out (leftpad (str ns) (len "000000000") "0"))
; %p     locale's equivalent of either AM or PM; blank if not known
                #\p (out (if (>= H 12) "PM" "AM"))
; %P     like %p, but lower case
                #\P (out (if (>= H 12) "pm" "am"))
; %q     quarter of year (1..4)
; %r     locale's 12-hour clock time (e.g., 11:11:04 PM)
                #\r (out (strftime "%I:%M:%S %p" secs))
; %R     24-hour hour and minute; same as %H:%M
                #\R (out (strftime "%H:%M" secs))
; %s     seconds since 1970-01-01 00:00:00 UTC
                #\s (out (str secs))
; %S     second (00..60)
                #\S (out (leftpad (str S) 2 "0"))
; %t     a tab
                #\t (out "\t")
; %T     time; same as %H:%M:%S
                #\T (out (strftime "%H:%M:%S" secs))
; %u     day of week (1..7); 1 is Monday
                #\u (out (str (aand (date-weekday (list Y m d)) (if (is it 0) 7 it))))
; %U     week number of year, with Sunday as first day of week (00..53)
                ;#\U (out (str (trunc:/ (date-yearday (list Y m d)) 7)))
; %V     ISO week number, with Monday as first day of week (01..53)
                ;#\V (out (str (trunc:/ (date-yearday (list Y m d)) 7)))
; %w     day of week (0..6); 0 is Sunday
                #\w (out (str (date-weekday (list Y m d))))
; %W     week number of year, with Monday as first day of week (00..53)
; %x     locale's date representation (e.g., 12/31/99)
                #\x (out (strftime "%m/%d/%y" secs))
; %X     locale's time representation (e.g., 23:13:48)
                #\X (out (strftime "%H:%M:%S" secs))
; %y     last two digits of year (00..99)
                #\y (out (cut (str Y) 2))
; %Y     year
                #\Y (out (str Y))
; %z     +hhmm numeric time zone (e.g., -0400)
                #\z (out "-0000")
; %:z    +hh:mm numeric time zone (e.g., -04:00)
                #\: (case (fmt (++ i))
                      #\z (out "-00:00")
; %::z   +hh:mm:ss numeric time zone (e.g., -04:00:00)
                      #\: (case (fmt (++ i))
                            #\z (out "-00:00:00")
; %:::z  numeric time zone with : to necessary precision (e.g., -04, +05:30)
                            #\: (case (fmt (++ i))
                                  #\z (out "-00")
                                  ; unknown
                                  (out "%:::" (fmt i)))))
; %Z     alphabetic time zone abbreviation (e.g., EDT)
                #\Z (out "GMT")
                ; unknown
                (do (out #\% (fmt i))))))))))


(def moment ((o secs (/ (msec) 1000)))
  (moment-ms (* 1000 secs)))

(defmemo moment-ms ((o ms (msec)))
  (with (secs (trunc (/ ms 1000))
         msecs (mod (trunc ms) 1000))
    (strftime (+ "+%Y-%m-%dT%H:%M:%S." (leftpad msecs 3 "0") "Z") secs)))

; For generating <pubDate> elements
(def rss-date ((o secs (seconds)))
  (strftime "+%a, %d %b %Y %H:%M:%S GMT" secs))

