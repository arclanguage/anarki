(load "scaffold.arc")

(deftem webscaff)

(addtem webscaff title
	'"Website Title")

(addtemscaff webscaff defvars
	     `(= ,rname!dir* (string "arc/" ',rname!s "/")  
		 ,rname!-maxid* 0  
		 ,rname!s* (table)))

(addtemscaff webscaff declare-tem
	     `(deftem ,(rname)
		id nil
		,@(mappend [list (car _) nil] rinfo)))

(addtemscaff webscaff load
	     `(def ,rname!s-load ()
		(each id (map [coerce _ 'int] (dir ,rname!dir*))
		      (= ,rname!-maxid* (max ,rname!-maxid* id)
			 (,rname!s* id) (temload ',(rname) (string ,rname!dir* id))))))

(addtemscaff webscaff save
	`(def ,rname!-save (p)
	   (save-table p (string ,rname!dir* (p 'id)))))

(addtemscaff webscaff accessor
	     `(def ,(rname) (id) (,rname!s* (errsafe (coerce id 'int)))))

(addtemscaff webscaff page-structure
	`(mac ,rname!page body
	      `(whitepage 
		(center
		 (widtable 600 
			   (tag b (link ,scaffold!title "../"))
			   (br 3)
			   ,@body
			   (br 3)
			   (w/bars (link ',rname!-archive)
				   (link ,(string "new " (rname)) ',rname!new)))))))

(addtemscaff webscaff view
	     `(defop ,rname!view req
		(aif (,(rname) (arg req "id")) 
		     (,rname!-singleton-page (get-user req) it) 
		     (notfound))))

(addtemscaff webscaff perma
	     `(def ,rname!-permalink (p) (string ,'(rname) "view?id=" (p 'id))))

(addtemscaff webscaff singleton-page
	     `(def ,rname!-singleton-page (user p) (,rname!page (,rname!-display user p))))

(addtemscaff webscaff short-part
	`(def ,rname!-short-part (p)
	   (p ',(caar rinfo))))

(addtemscaff webscaff long-part
	     `(def ,rname!-long-part (p)
		(p ',(caar:cdr rinfo))))

(addtemscaff webscaff display
	     `(def ,rname!-display (user p)
		(tag b (link (,rname!-short-part p) (,rname!-permalink p)))
		(when user
		  (sp)
		  (link "[edit]" (string ',(rname) "edit?id=" (p 'id))))
		(br2)
		(pr (,rname!-long-part p))))

(addtemscaff webscaff notfound
	     `(def ,rname!-notfound ()
		(,rname!page (pr "No such " ,(rname) "."))))

(addtemscaff webscaff new
	     `(defopl ,rname!new req
		(whitepage
		 (with (p (inst ',(rname))
			  user (get-user req))
		   (vars-form user
			      (list ,@(map [quasiquote (list ',cadr._ ',car._ (p ',car._) 't 't)] rinfo))
			      (fn (name val) (= (p name) val))
			      (fn () 
				  (,rname!-singleton-page user
							  (,rname!add user p))))))))

(addtemscaff webscaff add
	     `(def ,rname!add (user p)
		(= p!id (++ ,rname!-maxid*))
		(,rname!-save p)
		(= (,rname!s* (p 'id)) p)))

(addtemscaff webscaff edit
	     `(defopl ,rname!edit req
		(aif (,(rname) (arg req "id"))
		     (,rname!-edit-page (get-user req) it)
		     (,rname!-notfound))))

(addtemscaff webscaff edit-page
	     `(def ,rname!-edit-page (user p)
		(whitepage
		 (vars-form user
			    (list ,@(map [quasiquote (list ',cadr._ ',car._ (p ',car._) 't 't)] rinfo))
			    (fn (name val) (= (p name) val))
			    (fn () (,rname!-save p)
				(,rname!-singleton-page user p))))))

(addtemscaff webscaff archive
	     `(defop ,rname!-archive req
		(,rname!page
		 (tag ul
		      (each p (map ,(rname) (rev (range 1 ,rname!-maxid*)))
			    (tag li (link (,rname!-short-part p) (,rname!-permalink p))))))))

(addtemscaff webscaff main
	     `(defop ,rname!s req
		(let user (get-user req)
		  (,rname!page
		   (for i 0 4
			(awhen (,rname!s* (- ,rname!-maxid* i)) 
			  (,rname!-display user it)
			  (br 3)))))))

(addtemscaff webscaff init
	     `(let old-init ,sname!-init
		(def ,sname!-init ()
		  (if old-init (old-init))
		  (ensure-dir ,rname!dir*)
		  (,rname!s-load))))

(addtemscaff webscaff start-server
	     `(def ,sname!-start ()
		(,sname!-init)
		(asv)))
