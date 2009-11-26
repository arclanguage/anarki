;; JSON implementation for Scheme
;; See http://www.json.org/ or http://www.crockford.com/JSON/index.html
;;
;; Copyright (c) 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; JSON Structures are represented as vectors: #((symbol . value) (symbol . value) ...)
;; JSON Arrays are lists
;;
(module json mzscheme
  (require "packrat.ss")

  (provide json-read
	   json-write

	   hashtable->vector
	   vector->hashtable)

  (define (hashtable->vector ht)
    (list->vector
     (hash-table-map ht cons)))

  (define (vector->hashtable v)
    (let ((ht (make-hash-table)))
      (for-each (lambda (entry) (hash-table-put! ht (car entry) (cdr entry)))
		(vector->list v))
      ht))

  (define json-write
    (let ()
      (define (write-ht vec p)
	(display "{" p)
	(do ((need-comma #f #t)
	     (i 0 (+ i 1)))
	    ((= i (vector-length vec)))
	  (if need-comma
	      (display ", " p)
	      (set! need-comma #t))
	  (let* ((entry (vector-ref vec i))
		 (k (car entry))
		 (v (cdr entry)))
	    (cond
	     ((symbol? k) (write (symbol->string k) p))
	     ((string? k) (write k p)) ;; for convenience
	     (else (error "Invalid JSON table key in json-write" k)))
	    (display ": " p)
	    (write-any v p)))
	(display "}" p))

      (define (write-array a p)
	(display "[" p)
	(let ((need-comma #f))
	  (for-each (lambda (v)
		      (if need-comma
			  (display ", " p)
			  (set! need-comma #t))
		      (write-any v p))
		    a))
	(display "]" p))

      (define (write-any x p)
	(cond
	 ((hash-table? x) (write-ht (hashtable->vector x) p))
	 ((vector? x) (write-ht x p))
	 ((pair? x) (write-array x p))
	 ((symbol? x) (write (symbol->string x) p)) ;; for convenience
	 ((or (string? x)
	      (number? x)) (write x p))
	 ((boolean? x) (display (if x "true" "false") p))
	 ((void? x) (display "null" p))
	 (else (error "Invalid JSON object in json-write" x))))

      (lambda (x . maybe-port)
	(write-any x (if (pair? maybe-port) (car maybe-port) (current-output-port))))))

  (define json-read
    (let ()
      (define (generator p)
	(let ((ateof #f)
	      (pos (top-parse-position "<?>")))
	  (lambda ()
	    (if ateof
		(values pos #f)
		(let ((x (read-char p)))
		  (if (eof-object? x)
		      (begin
			(set! ateof #t)
			(values pos #f))
		      (let ((old-pos pos))
			(set! pos (update-parse-position pos x))
			(values old-pos (cons x x)))))))))

      (define parser
	(packrat-parser (begin
			  (define (white results)
			    (if (char-whitespace? (parse-results-token-value results))
				(white (parse-results-next results))
				(comment results)))
			  (define (skip-comment-char results)
			    (comment-body (parse-results-next results)))
			  (define (skip-to-newline results)
			    (if (memv (parse-results-token-value results) '(#\newline #\return))
				(white results)
				(skip-to-newline (parse-results-next results))))
			  (define (token str)
			    (lambda (starting-results)
			      (let loop ((pos 0) (results starting-results))
				(if (= pos (string-length str))
				    (make-result str results)
				    (if (char=? (parse-results-token-value results) (string-ref str pos))
					(loop (+ pos 1) (parse-results-next results))
					(make-expected-result (parse-results-position starting-results) str))))))
			  (define (interpret-string-escape results k)
			    (let ((ch (parse-results-token-value results)))
			      (k (cond
				  ((assv ch '((#\b . #\backspace)
					      (#\n . #\newline)
					      (#\f . #\page)
					      (#\r . #\return)
					      (#\t . #\tab))) => cdr) ;; we don't support the "u" escape for unicode
				  (else ch))
				 (parse-results-next results))))
			  (define (jstring-body results)
			    (let loop ((acc '()) (results results))
			      (let ((ch (parse-results-token-value results)))
				(case ch
				  ((#\\) (interpret-string-escape (parse-results-next results)
								  (lambda (val results)
								    (loop (cons val acc) results))))
				  ((#\") (make-result (list->string (reverse acc)) results))
				  (else (loop (cons ch acc) (parse-results-next results)))))))
			  (define (jnumber-body starting-results)
			    (let loop ((acc '()) (results starting-results))
			      (let ((ch (parse-results-token-value results)))
				(if (memv ch '(#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E))
				    (loop (cons ch acc) (parse-results-next results))
				    (let ((n (string->number (list->string (reverse acc)))))
				      (if n
					  (make-result n results)
					  (make-expected-result (parse-results-position starting-results) 'number)))))))
			  any)
			(any ((white '#\{ entries <- table-entries white '#\}) (list->vector entries))
			     ((white '#\[ entries <- array-entries white '#\]) entries)
			     ((s <- jstring) s)
			     ((n <- jnumber) n)
			     ((white (token "true")) #t)
			     ((white (token "false")) #f)
			     ((white (token "null")) (void)))
			(comment (((token "/*") b <- comment-body) b)
				 (((token "//") b <- skip-to-newline) b)
				 (() 'whitespace))
			(comment-body (((token "*/") w <- white) w)
				      ((skip-comment-char) 'skipped-comment-char))
			(table-entries ((a <- table-entries-nonempty) a)
				       (() '()))
			(table-entries-nonempty ((entry <- table-entry white '#\, entries <- table-entries-nonempty) (cons entry entries))
						((entry <- table-entry) (list entry)))
			(table-entry ((key <- jstring white '#\: val <- any) (cons key val)))
			(array-entries ((a <- array-entries-nonempty) a)
				       (() '()))
			(array-entries-nonempty ((entry <- any white '#\, entries <- array-entries-nonempty) (cons entry entries))
						((entry <- any) (list entry)))
			(jstring ((white '#\" body <- jstring-body '#\") body))
			(jnumber ((white body <- jnumber-body) body))
			))

      (define (read-any p)
	(let ((result (parser (base-generator->results (generator p)))))
	  (if (parse-result-successful? result)
	      (parse-result-semantic-value result)
	      (error "JSON Parse Error"
		     (let ((e (parse-result-error result)))
		       (list 'json-parse-error
			     (parse-position->string (parse-error-position e))
			     (parse-error-expected e)
			     (parse-error-messages e)))))))

      (lambda maybe-port
	(read-any (if (pair? maybe-port) (car maybe-port) (current-input-port))))))
)
