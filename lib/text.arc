; drainf and pump taken from cchooper's post on http://arclanguage.org/item?id=4220

(mac drainf (expr f)
  (w/uniq (gdone gres)
    `(let ,gdone nil
       (while (no ,gdone)
	      (let ,gres ,expr
		(if (is ,gres eof)
		    (= ,gdone t)
		    (,f ,gres)))))))

(def pump (in out (o binary nil))
  (with (err-flag t
		  inport  (if (isa in  'string) (instring  in)  in)
		  outport (if (isa out 'string) (outstring out) out))
    (after
      (do
	(if binary
	    (drainf (readb inport) [writeb _ outport])
	    (drainf (readc inport) [writec _ outport]))
	(= err-flag nil)
	(if (isa out 'string)
	    (inside outport)
	    out))
      (if (isa in  'string) (close inport))
      (if (isa out 'string) (close outport)))))

(def read-text-file (path)
  (w/infile i path (pump i "")))

(def read-text-list (path)
  (tokens (read-text-file path) #\newline))

(def write-text-list (lst path)
  (w/outfile outf path
	     (each elem (butlast lst)
	       (disp (+ elem "\n") outf))
	     (if lst (disp (last lst) outf))))
