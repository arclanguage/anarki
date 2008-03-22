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

(= oneline-sexp* 30)

; A pretty printer optimized for just prettifying sexpressions-
; Useful for prettying your sexp data before saving it to a file
; or showing it to a user.
;
; It has one config variable, oneline-sexp, which determines
; how long a line should be before it is split.  This is not a
; "hard" limit - the program won't break strings or other
; non-list structures to meet this limit.  Also, this limit
; does not count the indentation, so deeply nested sexps don't
; start becoming anorexically skinny to meet an artificial
; line break...  The goal here is to make the data easier to
; grok, not meet a hard column width.

(def ppr-sexp (sexp)
  (prn)
  (map [prn (string:n-of car._ #\space) cdr._]
       ((afn (sexp)
	  (withs (tw tostring:write
		     x tw.sexp)
	    (if (or (<= len.x oneline-sexp*) atom.sexp dotted.sexp) (list:cons 0 x)
		(with (y (map self sexp)
                         psh (fn (n lst)
                               (map [cons (+ n car._) cdr._] lst))
                         endit [let l (last _)
				 (= cdr.l (string cdr.l #\)))
                                 _])
		  (if (acons:car sexp) (let q (psh 1 (apply join (cdr:car y) cdr.y))
					 (cons (cons 0 (string #\( 
							       (cdr:caar y) 
							       (unless q 
								       #\))))
					       (when q 
						 (endit q))))
		      (is 1 (len sexp)) (list (cons 0 (string #\( (tw car.sexp) #\))))
		      (and (is 2 (len sexp)) (is 1 (len cadr.y))) (list (cons 0 (string #\( (tw car.sexp) #\space (tw cadr.sexp) #\))))
		      (let b (string #\( (tw car.sexp) #\space)
			(cons (cons 0 (string b (cdr:car:car:cdr y)))
			      (endit:psh len.b (apply join (cdr:cadr y) cddr.y)))))))))
	sexp)))
