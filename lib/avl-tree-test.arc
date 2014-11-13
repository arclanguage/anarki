;; Fuzz testing for AVL trees - by Pauan - http://arclanguage.org/item?id=18936

(load "lib/avl-tree.arc")

; Depth must be correct
(def verify-depth (tree)
  (is (depth tree)
      (inc:max (depth tree!lf)
               (depth tree!rt))))

; Depth must not vary by more than 1
(def verify-height (tree)
  (let diff (- (depth tree!lf)
               (depth tree!rt))
    (in diff -1 0 1)))

(def verify-tree (tree tests)
  ; Empty trees are always valid
  (or (no tree)
      (and (verify-depth  tree)
           (verify-height tree)

           ; Verify that:
           ;   #1 all keys to the left  are lower   than
           ;   #2 all keys to the right are greater than
           (all (fn (f) (f tree)) tests)
           (verify-tree tree!lf (cons (fn (x) (< x!dt tree!dt)) tests))
           (verify-tree tree!rt (cons (fn (x) (> x!dt tree!dt)) tests)))))

(def verify (tree)
  (if (verify-tree tree nil)
    tree
    (err "bad tree")))

; Get a list of numbers shuffled in a random order.
(ero "Generating 1000 values")
(= shuffle (compose $.ac-niltree $.shuffle $.ar-denil-last))
(= numbers (shuffle (range 0 1000)))

; And now let's create an AVL tree by inserting the numbers into it, in sorted
; order.
(ero "Adding them to a tree")
(= n 0)
(= tree
   (foldl (fn (x y)
            (if (is 0 (mod n 100)) ero.n)
            (++ n)
            (verify (ainsert < y x)))
          nil
          numbers))

; Now let's try removing the elements from the tree.
(ero "Removing everything from the tree")
(= n 0)
(= tree
  (foldl (fn (x y)
            (if (is 0 (mod n 100)) ero.n)
            (++ n)
           (verify (aremove < y x)))
         tree
         numbers))
