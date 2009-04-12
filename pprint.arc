; Pretty-Printing.  Spun off 4 Aug 06.

; todo: indentation of long ifs; quasiquote, unquote, unquote-splicing
           
(= bodops* (fill-table (table)
   '(let 2 with 1 while 1 def 2 fn 1 rfn 2 afn 1
     when 1 unless 1 after 1 whilet 2 for 3 each 2 whenlet 2 awhen 1
     whitepage 0 tag 1 form 1 aform 1 aformh 1 w/link 1 textarea 3
   )))

(= oneline* 35) ; print exprs less than this long on one line

; If returns nil, can assume it didn't have to break expr.
  
(def ppr (expr (o col 0) (o noindent nil))
  (if (or (atom expr) (dotted expr))
       (do (unless noindent (sp col))
           (write expr)
           nil)
      (is (car expr) 'quote)
       (do (unless noindent (sp col))
           (pr "'")
           (ppr (cadr expr) (+ col 1) t))
      (bodops* (car expr))
       (do (unless noindent (sp col))
           (let whole (tostring (write expr))
             (if (< (len whole) oneline*)
                 (do (pr whole) nil)
                 (ppr-progn expr col noindent))))
      (do (unless noindent (sp col))
          (let whole (tostring (write expr))
            (if (< (len whole) oneline*)
                (do (pr whole) nil)
                (ppr-call expr col noindent))))))

(def ppr-progn (expr col noindent)
  (lpar)
  (let n (bodops* (car expr))
    (let str (tostring (write-spaced (firstn n expr)))
      (unless (is n 0) (pr str) (sp))
      (ppr (expr n) (+ col (len str) 2) t))
    (map (fn (e) (prn) (ppr e (+ col 2)))
         (nthcdr (+ n 1) expr)))
  (rpar)
  t)
             
(def ppr-call (expr col noindent)
  (lpar)
  (let carstr (tostring (write (car expr)))
    (pr carstr)
    (if (cdr expr)
        (do (sp)
            (let broke (ppr (cadr expr) (+ col (len carstr) 2) t)
              (pprest (cddr expr)
                      (+ col (len carstr) 2)
                      (no broke)))
            t)
        (do (rpar) t))))
       
(def pprest (exprs col (o oneline t))
  (if (and oneline
           (all (fn (e)
                  (or (atom e) (and (is (car e) 'quote) (atom (cadr e)))))
                exprs))
      (do (map (fn (e) (pr " ") (write e))
               exprs)
          (rpar))
      (do (when exprs
            (each e exprs (prn) (ppr e col)))
          (rpar))))
                
(def write-spaced (xs)
  (when xs
    (write (car xs))
    (each x (cdr xs) (pr " ") (write x))))
  
(def sp ((o n 1)) (repeat n (pr " ")))
(def lpar () (pr "("))
(def rpar () (pr ")"))

