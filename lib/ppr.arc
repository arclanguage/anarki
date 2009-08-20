(= pprsyms* (fill-table (table) 
			'(quote "'" 
			  quasiquote "`"
			  unquote ","
			  unquote-splicing ",@")))

(def sp ((o n 1))
  " Print a number of spaces. "
  (repeat n (pr " ")))

(def print-spaced (xs)
  " Print the expressions in the list separated by spaces. "
  (when xs
    (print car.xs)
    (each x cdr.xs (sp) print.x)))

(def print (x)
  " Print an expression on one line, replacing quote, unquote,
    quasiquote, unquote-splicing, and make-br-fn with their respective symbols. " 
  (do (aif (or atom.x dotted.x)
	     (do write.x nil)
	   (pprsyms* car.x)
	     (do pr.it
	       (print cadr.x)
	       nil)
	   (is car.x 'make-br-fn)
	     (do (pr "[") (print-spaced cadr.x) (pr "]") nil)
	   (do (pr "(") print-spaced.x (pr ")") nil))
      x))

(= oneline* 45)

(def len (x (o c 0))
  " Measures the length of a string, vector, table, list or dotted list. "
  (if (isa x 'string) ($.string-length x)
      (isa x 'vec) ($.vector-length x)
      (isa x 'table) ($.hash-table-count x)
      (and atom.x x) (+ c 1)
      acons.x (len cdr.x (+ c 1))
      c))

(mac indent (col . body)
  `(do (unless noindent sp.col)
       ,@body))

(mac ppr-sub body
  `(indent col
      (let whole (tostring print.x)
	(if (< len.whole oneline*)
	    (do pr.whole nil)
	    (do ,@body t)))))

(def indent-pairs (xs (o col 0))
  (let l (apply max 0 (map len:tostring:print:car (keep cdr pair.xs)))
    (on x pair.xs
        (if (~is index 0)
            (do (prn)
                (sp col)))
        (let str (tostring:print car.x)
          (if cdr.x
              (do pr.str
                  (sp:- l len.str -1)
                  (ppr cadr.x (+ col 1 l) t))
              ; lone tail expression
              (do (sp (+ l 1))
                  (ppr car.x (+ col (+ l 1)) t)))))))

(def indent-block (xs (o col 0))
  (each x xs (prn) (ppr x col)))

(def indent-mac (xs (o args 0) (o col 0))
  (print-spaced (firstn args xs))
  (indent-block (nthcdr args xs) (+ col 2)))

(def indent-basic (xs l (o col 0))
  (if (all [or atom._ (and (is car._ 'quote) (atom cadr._))]
	   xs)
      print-spaced.xs
      (do (ppr car.xs (+ col 2 l) t)
	  (indent-block cdr.xs (+ col 2 l)))))

(def indent-wave (xs (o col 0))
  (do (ppr car.xs col t)
      (on x cdr.xs
	  (prn)
	  (ppr x (+ col (* 2 (mod (+ index 1) 2)))))))

(= ifline* 20)

(def indent-if (l)
  (fn (xs (o col 0)) 
      (if (< len.xs 4)
	    (on x xs 
		(if (~is index 0) (prn))
		(ppr x (+ col 2 l) (is index 0)))
	  (all [< (len:tostring print._) ifline*]
	       pair.xs)
	    (indent-pairs xs (+ col 2 l))
	  (indent-wave xs (+ col 2 l)))))

(def indent-with (l)
  (fn (xs (o col 0))
      (pr "(")
      (indent-pairs car.xs (+ col 3 l))
      (pr ")")
      (indent-block cdr.xs (+ col 3))))

(def indent-def (xs (o col 0))
  (print-spaced (firstn 2 xs))
  (if (isa xs.2 'string)
      (do (prn)
	  (sp (+ col 2))
	  (pr #\" xs.2 #\")
	  (indent-block (nthcdr 3 xs) (+ col 2)))
      (indent-block (nthcdr 2 xs) (+ col 2))))

(def indent-case (n)
  (fn (xs (o col 0))
      (print-spaced:firstn n xs)
      (prn)
      (sp (+ col 2))
      (indent-pairs (nthcdr n xs) (+ col 2))))

(= indent-rules* 
   (fill-table (table)
     `(if      ,(indent-if 2)
       aif     ,(indent-if 3)
       with    ,(indent-with 4)
       withs   ,(indent-with 5)
       def     ,indent-def
       mac     ,indent-def
       do      ,[indent-basic _ 2 _2]
       and     ,[indent-basic _ 3 _2]
       or      ,[indent-basic _ 2 _2]
       nor     ,[indent-basic _ 3 _2]
       case    ,(indent-case 1)
       caselet ,(indent-case 2)
       fn      ,[indent-mac _ 1 _2])))

(def ppr (x (o col 0) (o noindent nil))
  " Pretty print. This function displays arc code with proper
    indenting and representation of syntax. "
  (aif (or atom.x dotted.x)		;just print the expression if it's an atom or dotted list
         (indent col
	   print.x
	   nil)
       (is car.x 'make-br-fn)		;if the expression is a br-fn, print the brackets and then the contents
         (ppr-sub
	   (pr "[")
	   (ppr cadr.x (+ col 1) t)
	   (pr "]"))
       (pprsyms* car.x)
         (ppr-sub
	   pr.it
	   (ppr cadr.x (+ col len.it) t))
       (ppr-sub
	 (pr "(")
	 (withs (proc car.x
		 args sig.proc
                 n    len.args
		 str  (tostring:print proc)
		 l    len.str
		 xs   cdr.x)
	   (if (isa proc 'cons)
	       (do (ppr proc (+ col 1) t)
		   (indent-block xs (+ col 1)))
	       (do pr.str
		   (when xs
		     (sp)
		     (aif indent-rules*.proc
			    (it xs col)
			  (and (isa proc 'sym) (bound proc) (isa (eval proc) 'mac))
			    (if (or dotted.args (and args (~acons args)))
				(indent-mac xs (- len.args 1) col)
				(indent-mac xs 0 col))
			  (indent-basic xs l col)))))
	   (pr ")")))))