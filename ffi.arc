(mac cdef (name cname out-type in-types (o finalizer))
	(if finalizer
		(w/uniq (cfun res)
			`(= ,name (fn args 
				(let ,res (apply (get-ffi-obj ,cname ffi (cfn (list ,@in-types) ,out-type)) args)
					(cfinalize ,res ,finalizer)
					,res))))
		`(= ,name (get-ffi-obj ,cname ffi (cfn (list ,@in-types) ,out-type)))))


(mac w/ffi (name . body) ; catches ffi
	`(let ffi (ffi-lib ,name)
		,@body))


(mac w/inline (code . body) ; catches ffi
	(w/uniq (u filename f)
		`(withs
			(,u        (uniq)
			 ,filename (string ,u ".c"))
			(w/outfile ,f ,filename
				(w/stdout ,f (prn ,code)))
			(prn:tostring:system:string "gcc -O3 -Wall --pedantic --ansi --shared -o " ,u ".so " ,u ".c")
			;(system:string "rm -f " ,u ".c")

			(let ffi (ffi-lib:string ,u)
				,@body))))


(mac w/del (var value del-fn . body)
	`(let ,var ,value
		,@body
		(,del-fn ,var)))

