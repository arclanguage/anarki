; array library for arc0. 
;
; An array is an indexed function, where the indices are 
; non-negative integers 
;
; Example: 
;   (let a (array '(4 5) 0) 
;      (<- (a 0 0) 'something) ; sets element 0,0  
;      (array>list a))
;
; One pecularity with the library is that the indexing is 
; cyclic, such that for a vector of length N, (v N) and (v 0)
; is the same element. If you like generate an out-of-bounds exeption instead, 
; just replace the indexing function.


; An array is a function which tells it's an array 
; when called without arguments
(def array? (a)
  (if (and (isa a 'fn) (is 'array (car (a)))) t))

; About the setter, <-. The basics are stolen from arc.arc's =. 
; Unlike =, <- takes the setter from the function,
; and it will work on any object that returns a setter 
; form (fn (val . vars)). So far it does not work on normal hash tables.
(def expand<- (place val)
  (if (isa place 'sym)
      `(set ,place ,val)
      (cons `(,(car place) 'setter) (cons val (cdr place)))))
(def expand<-list (terms)
  `(do ,@(map (fn ((p v)) (expand<- p v))
                  (pair terms))))
(mac <- args 
  (expand<-list args))
       
; Returns a function that maps 
; an index list to a number.
(def make-indexer (dim) 
  (fn idx
     (let rec (afn (dim idx)
                   (+ (mod (car idx) (car dim))
                      (* (car dim)
                         (if (no:cdr dim)
                             0
                             (self (cdr dim) (cdr idx))))))
         (rec dim idx))))

; Integer division.
(def div (a b)
  (/ (- a (mod a b)) b))

; Returns a funcion that maps
; a number to an index list
(def make-inverse-indexer (dim)
  (fn (i)
     (withs (idx nil 
             rec (afn (dim i)
                   (push (mod i (car dim)) idx)
                   (when (cdr dim)
                      (self (cdr dim) (div i (car dim))))))
        (rec dim i)
        (rev idx))))

; Makes an array. The store is a hash table.
(def make-array-wrapper (dim store)
  (let indexer (make-indexer dim)
    (annotate 'array
              (fn args
                  (if (no args) 
                      `(array ,@dim)
                      (let x (car args) 
                        (if (is x 'dim) dim
                            (is x 'rank) (len dim)
                            (is x 'setter) (fn (val . x)
                                               (if (isa (car x) 'cons)
                                                   (= (store (apply indexer (car x))) val)
                                                   (= (store (apply indexer x)) val)))
                            (is x 'store) store
                            (is x 'indexer) indexer
                            (is x 'inverse-indexer) (make-inverse-indexer dim) 
                            (store (apply indexer args)))))))))

; Creates a new array with the given dimensions.
(def array (dim (o contents nil))
  (let a (make-array-wrapper (if (isa dim 'cons) (copy dim) (list dim)) (table))
     (when contents 
       (if (isa contents 'cons)
         (set-array! a contents)
         (fill-array! a contents)))
     a))

; Creates a array with different indexing, but shared elements. 
(def share-array! (a dim)
  (make-array-wrapper (if (isa dim 'cons) (copy dim) (list dim)) (a 'store)))

(def size (a)
  (reduce * (a 'dim)))

(def fill-array! (a val)
  (let b (share-array! a (size a))
     (for i 0 (- (size b) 1)
        (<- (b i) val)))
  a)

(def copy-array (a)
  (withs (N (size a)
          a1 (share-array! a N)
          b (array (a 'dim))
          b1 (share-array! b N))
     (for i 1 N ; he he, exploit that it's cyclic
       (<- (b1 i) (a1 i)))
     b))

(def array>list (a)
  (let printer (rfn printer (idx dim)
                     (let result nil
                     (if (no dim) 
                         (= result (apply a (rev idx)))
                         (for i 0 (- (car dim) 1)
                            (= result (+ result (list (printer (cons i idx) (cdr dim)))))))
                      result))
     (list 'array (a 'dim) (printer nil (a 'dim)))))

(def set-array! (a tree)
  (let rec (rfn rec (rank lst idx)
                     (if (is rank 0)
                         (<- (a (rev idx)) lst)
                         (let count -1
                           (each l lst
                              (rec (- rank 1) l (cons (++ count) idx))))))
  (rec (a 'rank) tree nil)
  a))

(redef sref (com val ind)
  (if (no (isa com 'array)) (old com val ind)
      ((com 'setter) val ind)))

; Example: matrix inversion.
(def inv (a)
  (withs (a (copy-array a)
          N ((a 'dim) 0)
          tmp 0)
    (for i 0 (- N 1) 
      (<- tmp (a i i))
      (for j 0 (- N 1)
        (<- (a i j) (if (is i j)
                        (/ (a i j))
                        (/ (a i j) tmp))))
      (for j 0 (- N 1)
        (unless (is i j)
          (<- tmp (a j i)
              (a j i) 0)
          (for k 0 (- N 1)
            (<- (a j k)
                (- (a j k) (* tmp (a i k))))))))
    a))


