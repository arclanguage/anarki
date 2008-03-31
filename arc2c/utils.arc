; utilities

(def diff (s1 s2)
	(if
		(no s1)
			'()
		(mem (car s1) s2)
			(diff (cdr s1) s2)
			(cons (car s1) (diff (cdr s1) s2))))

(def union (s1 s2)
	(if
		(no s1)
			s2
		(mem (car s1) s2)
			(union (cdr s1) s2)
			(cons (car s1) (union (cdr s1) s2))))

(def union-multi (sets)
	(foldl union '() sets))

(def foldl (f base lst)
	(if (no lst)
		base
		(foldl f (f base (car lst)) (cdr lst))))

(def liststr (lst)
	(let result ""
		(each elt lst
			(if (alist elt)
				(++ result (liststr elt))
				(++ result (string elt))))
		result))

;------------------------------------------------------------------------------

; free variables

(def fv (ast)
	(if
		(aref ast)
			(list ast!var)
		(aset ast)
			(union (fv (car ast!subx)) (list ast!var))
		(alam ast)
			(diff (fv (car ast!subx)) ast!params)
		(aquote ast)
			nil
			(union-multi (map fv ast!subx))))

