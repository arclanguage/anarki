; statistics. 06 Oct 2010.

; Ali Moeeny

(def diffSquared (x y) 
     		 (apply + (map [* (- _ y)(- _ y)] x)))

(def sumSquared (x y) 
     	   (apply + (map * x y)))

(def sumDiffMultiplied (x y) 
       (with (mx (avg x) my (avg y))
       (reduce + 
       	       (map * 
	       	    (map [- _ mx] x)
		    (map [- _ my] y)))))



(def pearsonsCorrelation (x y) 
     (/ (sumDiffMultiplied x y) 
        (sqrt 
	      (* 
	      	 (diffSquared 
		 	 x 
			 (avg x)) 
	         (diffSquared y 
		 	 (avg y)))))
)