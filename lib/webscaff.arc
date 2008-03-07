(load "lib/scaffold.arc")

(deftem webscaff)

(addtem webscaff title
	'"Website Title")

(addtemscaff webscaff defvars
	     `(= ,ename!dir* (string "arc/" ',ename!s "/")  
		 ,ename!-maxid* 0  
		 ,ename!s* (table)))

(addtemscaff webscaff declare-tem
	     `(deftem ,(ename)
		id nil
		,@(mappend [list (car _) nil] entity)))

(addtemscaff webscaff load
	     `(def ,ename!s-load ()
		(each id (map [coerce _ 'int] (dir ,ename!dir*))
		      (= ,ename!-maxid* (max ,ename!-maxid* id)
			 (,ename!s* id) (temload ',(ename) (string ,ename!dir* id))))))

(addtemscaff webscaff save
	`(def ,ename!-save (p)
	   (save-table p (string ,ename!dir* (p 'id)))))

(addtemscaff webscaff accessor
	     `(def ,(ename) (id) (,ename!s* (errsafe (coerce id 'int)))))

(addtemscaff webscaff page-structure
	`(mac ,ename!page body
	      `(whitepage 
		(center
		 (widtable 600 
			   (tag b (link ,',scaffold!title "../"))
			   (br 3)
			   ,@body
			   (br 3)
			   (w/bars (link ',',ename!-archive)
				   (link ,',(string "new " (ename)) ',',ename!new)))))))

(addtemscaff webscaff view
	     `(defop ,ename!view req
		(aif (,(ename) (arg req "id")) 
		     (,ename!-singleton-page (get-user req) it) 
		     (,ename!-notfound))))

(addtemscaff webscaff perma
	     `(def ,ename!-permalink (p) (string ',(ename) "view?id=" (p 'id))))

(addtemscaff webscaff singleton-page
	     `(def ,ename!-singleton-page (user p) (,ename!page (,ename!-display user p))))

(addtemscaff webscaff short-part
	`(def ,ename!-short-part (p)
	   (p ',(caar entity))))

(addtemscaff webscaff long-part
	     `(def ,ename!-long-part (p)
		(p ',(caar:cdr entity))))

(addtemscaff webscaff display
	     `(def ,ename!-display (user p)
		(tag b (link (,ename!-short-part p) (,ename!-permalink p)))
		(when user
		  (sp)
		  (link "[edit]" (string ',(ename) "edit?id=" (p 'id))))
		(br2)
		(pr (,ename!-long-part p))))

(addtemscaff webscaff notfound
	     `(def ,ename!-notfound ()
		(,ename!page (pr "No such " ,(ename) "."))))

(addtemscaff webscaff new
	     `(defopl ,ename!new req
		(whitepage
		 (with (p (inst ',(ename))
			  user (get-user req))
		   (vars-form user
			      (list ,@(map [quasiquote (list ',cadr._ ',car._ (p ',car._) 't 't)] entity))
			      (fn (name val) (= (p name) val))
			      (fn () 
				  (,ename!-singleton-page user
							  (,ename!add user p))))))))

(addtemscaff webscaff add
	     `(def ,ename!add (user p)
		(= p!id (++ ,ename!-maxid*))
		(,ename!-save p)
		(= (,ename!s* (p 'id)) p)))

(addtemscaff webscaff edit
	     `(defopl ,ename!edit req
		(aif (,(ename) (arg req "id"))
		     (,ename!-edit-page (get-user req) it)
		     (,ename!-notfound))))

(addtemscaff webscaff edit-page
	     `(def ,ename!-edit-page (user p)
		(whitepage
		 (vars-form user
			    (list ,@(map [quasiquote (list ',cadr._ ',car._ (p ',car._) 't 't)] entity))
			    (fn (name val) (= (p name) val))
			    (fn () (,ename!-save p)
				(,ename!-singleton-page user p))))))

(addtemscaff webscaff archive
	     `(defop ,ename!-archive req
		(,ename!page
		 (tag ul
		      (each p (map ,(ename) (rev (range 1 ,ename!-maxid*)))
			    (tag li (link (,ename!-short-part p) (,ename!-permalink p))))))))

(addtemscaff webscaff main
	     `(defop ,ename!s req
		(let user (get-user req)
		  (,ename!page
		   (for i 0 4
			(awhen (,ename!s* (- ,ename!-maxid* i)) 
			  (,ename!-display user it)
			  (br 3)))))))

(addtemscaff webscaff init
	     `(let old-init ,sname!-init
		(def ,sname!-init ()
		  (if old-init (old-init))
		  (ensure-dir ,ename!dir*)
		  (,ename!s-load))))

(addtemscaff webscaff start-server
	     `(def ,sname!-start ()
		(,sname!-init)
		(asv)))
