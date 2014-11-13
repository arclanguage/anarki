;; AVL trees - by waterhouse - http://arclanguage.org/item?id=14181
;
; AVL trees are augmented binary search trees: each node, in addition to having
; a datum and left and right branches, contains the depth of the subtree that it
; represents. This depth is equal to:
;
;   1 + max(depth(left branch), depth(right branch))
;
; It's useful for keeping the tree balanced.
; 
;; Motivation
;
; We start by considering a binary search tree. If this tree is balanced--all
; the leaves are at the same depth, or within 1 of the same depth--then the
; depth of the tree with n elements is between log_2(n) and 1+log_2(n). Which
; means that lookups take O(log n) time, which is good. And we can insert an
; element in log n time, too, by looking in the tree for where that element
; would be, and adding it to the bottom. However, this may unbalance the tree;
; if our tree has the numbers from 1 to 15, and we insert the numbers -1
; through -20, then the tree becomes massively unbalanced on the left side and
; we no longer have O(log n) anything.
; 
; The next step is to say, hmm, well, perhaps we could rebalance the tree when
; we insert something. One way to do that is to construct a new balanced tree
; from the elements of the old one, plus the new element; this works, but it
; takes O(n) time for every insertion, which is really bad.
; 
; Now, someone at some point had the genius idea that maybe you could only
; perform local rebalancings. If, in the below case, we've just inserted
; something into the subtree A, and A has depth x+1, C depth x, and E depth x,
; then B has depth x+2, and this tree isn't balanced. We can shift around a
; couple of nodes, as illustrated, and now the two branches of the main tree
; both have depth x+1, so we're happy.
; 
;        D              B
;     B     E  -->   A     D
;   A   C                C   E
; 
; Doing this requires knowing the depth at each node. In general, computing the
; depth of a binary tree means following every branch all the way to the
; bottom--i.e. it is O(n).[1] However, if every node contains its depth, then
; it's an instant lookup. This little rebalancing operation was O(2), so the
; whole insertion took O(log n).
; 
; This looks like a promising approach, if we can get it to work right. If you
; do some fooling around and thinking, you may figure out that we can require
; all nodes to be balanced (so that [depth(left) - depth(right)] = 1, 0, or -1),
; and enforce this by rebalancing from the bottom up every time we insert
; something. And note that if the maximum difference between left and right
; branches is 1, then we can prove by induction that the minimum number of
; elements of an AVL tree with depth n is Fibonacci of something (specifically
; Fib(n+2)-1), which gives O(log_phi(n)) as an upper bound on the depth of an
; AVL tree with n elements.
; 
; ------
; 
;; Implementation.
;
; An AVL node contains a datum, a left and a right branch, and a depth.[2]
; I'll use nil as an empty AVL node.

  (def node (d x y)
    (obj dt  d  ; datum, left, right, depth
         lf  x
         rt  y
         dp  (inc:max depth.x depth.y)))

  (def depth (node)
    (if node
        node!dp
        0))

; Using the function "node", we can construct AVL trees, as in (node 2 (node 1
; nil nil) (node 3 nil nil)). Now we need to handle rebalancing. We could do
; this by modifying nodes, but I find it easier to just construct new ones.[3]
;
;               D               B
;            B     E  -->   A       D
;          A   C          1   2   C   E
;         1 2                           
;        
;
;               D               C
;            B     E  -->   B       D
;          A   C          A   1   2   E
;             1 2                      
;  
; (and mirror images)

; Whenever [depth(left) - depth(right)] isn't -1, 0, or 1, we need to fix
; that. An imbalance can only happen as the result of a single insertion or
; removal; this means the depth mismatch of the two branches can be at most 2;
; for convenience, let's say the left branch (B) has depth x+2, while the
; right (E) has depth x. (Consult mirror images for the opposite case.) Since
; we rebalance things from the bottom up, B and E will, individually, be
; balanced; in particular, this means that the left and right branches of B (A
; and C) can have depths of, respectively, x+1 and x; x+1 and x+1; or x and
; x+1. The top chart above illustrates the first case, the bottom chart the
; third case; the second case can be handled as either the first or the third.
; (All we need to do is make sure no nodes come out unbalanced.)
;
; So here we go. In the following function, x and y correspond to B and E in
; the charts, and the node to be constructed corresponds to D.[4]

(def node/r (d x y) ;like node but rebalances
  (if (> depth.x (inc depth.y))
        (if (< (depth x!lf) (depth x!rt))
          (node x!rt!dt (node x!dt x!lf x!rt!lf)
                (node d x!rt!rt y))
          (node x!dt x!lf (node d x!rt y)))
      (> depth.y (inc depth.x))
        (if (< (depth y!rt) (depth y!lf))
          (node y!lf!dt (node d x y!lf!lf)
                (node y!dt y!lf!rt y!rt))
          (node y!dt (node d x y!lf) y!rt))
      :else
        (node d x y)))

; Note that, up to this point, we haven't needed to compare any elements, and
; in fact I had forgotten I would have to do that. This realization pleases
; me. However, we do need to compare things when inserting or removing.

(def ainsert (less x tree)
  (if no.tree
        (node x nil nil)
      (less x tree!dt)
        (node/r tree!dt
                (ainsert less x tree!lf)
                tree!rt)
      :else
        (node/r tree!dt
                tree!lf
                (ainsert less x tree!rt))))

(def aremove (less test tree)
  (if no.tree
        (err "aremove: failed to find" test "in" tree)
      (is test tree!dt)
        (amerge tree!lf tree!rt)
      (less test tree!dt)
        (node/r tree!dt
                (aremove less test tree!lf)
                tree!rt)
      :else
        (node/r tree!dt
                tree!lf
                (aremove less test tree!rt))))

(def amerge (a b) ; not a general merge; assumes [all of a] ≤ [all of b]
  (if no.a
        b
      no.b
        a
      (< depth.a depth.b)
        (node/r b!dt
                (amerge a b!lf)
                b!rt)
      :else
        (node/r a!dt
                a!lf
                (amerge a!rt b))))

;; Time for a pretty picture:
;
; http://i.imgur.com/9CJ4d.png
;
; Here's what an AVL tree looks like when we insert 10 random numbers followed
; by 21 consecutive large numbers. I drew it with Graphviz, which is some
; pretty snazzy (and open source) graph-drawing software. To generate graphs:
;
;   arc> (= u nil)
;   nil
;   arc> (repeat 10 (= u (ainsert < rand.100 u)))
;   nil
;   arc> (for i 200 220 (= u (ainsert < i u)))
;   nil
;   arc> ashow.u
;   #<thread: ashow>

(def agviz (tree)
  (pr "digraph g {")
  (when tree
    (pr "node [shape = record,height=.1];")
    (xloop (tree tree name "node")
      (pr name "[label = \"{" tree!dt "|depth: " depth.tree "}\"];")
      (pr "\"" name "\";")
      (with (a (string name "0") b (string name "1"))
        (when tree!lf
          (pr "\"" name "\":sw -> \"" a "\";")
          (next tree!lf a))
        (when tree!rt
          (pr "\"" name "\":se -> \"" b "\";")
          (next tree!rt b)))))
  (pr "}"))

; this writes Graphviz code, creates a PNG, and opens it. if "name" is
;  unspecified, it deletes these files after you close your PNG viewer.
; uses MacOSX-specific "open -nW" and requires graphviz
(def ashow (tr (o name nil)) 
  (let fname (or name (tmpfile))
    (tofile fname agviz.tr)
    (thread:system:string
       "dot -Tpng " fname " -o " fname ".png && open -nW " fname ".png"
       (if no.name
         (string " && rm " fname " " fname ".png")))))

; -------
; 
;; Uses and extensions.
; 
; You can use an AVL tree to implement a priority queue. (Priority queue:
; sorted queue, supports "insert" and "get and delete first element". In this
; case, both of these are O(log n).) In fact, this was what originally
; impelled me to do this. The "insert-sorted" procedure in arc.arc, which I
; mentioned in footnote [2] [my implementation is different but equivalent],
; obviously wants to treat sorted lists like they're priority queues, but the
; insertion is O(n).
; 
; I haven't (yet) implemented destructive rebalancing, insertion, or removal.
; These would take O(log n) time as well, but would generate zero unnecessary
; garbage (instead of O(log n)). Depending on what you're doing, that
; difference might be desirable (I'd want it for my priority queue), or it
; might be unimportant, or you might actually need the side-effect-free
; version.
; 
; You can replace the "datum" slot in each node with "key" and "value" slots,
; perhaps adding a third "key-hash" slot so you can sort diverse or compound
; keys. This might be useful for, say, string interning, or more generally for
; a hash table substitute. Paul Graham says in the Arc tutorial: "I once
; thought alists were just a hack, but there are many things you can do with
; them that you can't do with hash tables, including sort them, build them up
; incrementally in recursive functions, have several that share the same tail,
; and preserve old values." Funnily, you can do all of these things with AVL
; trees (+key +value +key-hash) and do them in O(log n) time, except for one:
; if you sort them (presumably according to something other than the hash
; value, which they'd otherwise be sorted by), then lookups will degrade to
; O(n).
; 
; You can add a "count" slot to each node (and update it when
; inserting/removing/rebalancing), representing the number of elements in the
; whole subtree. count(nil) = 0, count(x) = count(x!left) + 1 +
; count(x!right). This would give you O(log n) access to the kth element,
; O(log n) insertion or deletion at the kth position (if you want it), plus an
; O(1) "length" function. This seems like a good general-purpose "list",
; actually--it beats arrays at "insertion or deletion at the kth position",
; and it beats normal (linked) lists at everything except
; lookup/insertion/removal at the head. And if we keep it sorted, then a
; priority queue becomes just a specific way of using it (you only use
; "insert-sorted" and "access/delete 0th element").
; 
; Continuing with the "count" slot, "append" seems it'd be O(log n). "reverse"
; would be O(n), but if you really wanted it, you could put in an extra slot,
; a flag that said "Consider this tree reversed"--it'd be trippy, it'd require
; writing reversed versions of everything--which would handle nested reverse
; flags--and when, say, you performed the rebalancing in the top chart and B
; was reversed but E wasn't, then, in the resulting tree, A and C would get
; "reversed" flags (and they'd switch places) and B would lose its flag...
; it'd be pretty ridiculous, but I believe it would work perfectly, and
; reversal would be O(1). It'd be awesome.
; 
; You can add a "parent" slot, a pointer to the parent node (probably nil for
; the root node). This commits you to destructive insertion/removal; since any
; part of the tree can be reached from any other part, nondestructively adding
; something would require copying the whole tree. What does +parent buy you?
; Well, I thought, it lets insertion (and probably removal) run with constant
; memory: you go to the bottom, insert your element, then, if needed, go up to
; the parent and update depth/rebalance (and if you do that, check if the
; parent of that needs updating; if not, terminate). The alternative (which my
; nondestructive code currently uses, and which destructive code without
; +parent would use) is to temporarily store all the nodes on the path to the
; bottom (implicitly, in a call stack). Upon further thought, this might not
; make much of a difference, because this call stack gets exactly as deep as
; the tree, which is approximately log_phi(n), which is 43 when n is 1
; billion. I don't know, it might be worth it. It could likewise be used for
; iterating along the tree.
; 
; -------
; 
; The point is, there's a lot of cool stuff you can do with AVL trees. Some of
; it is even useful (like the priority queue, and maybe the general-purpose
; list). It seems there's lots of room for customizing your AVL tree to fit
; your problem.
; 
; A wise man apparently said, "Data dominates. If you've chosen the right data
; structures and organized things well, the algorithms will almost always be
; self-evident. Data structures, not algorithms, are central to programming."
; That seems to be true here: with every incarnation of AVL trees I've listed
; above, the ways to manipulate it have seemed clear to me (at least in
; specification; the implementation may take a bit of thinking).
; 
; -------
; 
; [1] You might think, "Well, if I keep the tree balanced, then I can just
; follow one path to the bottom, and that tells me the depth... up to an error
; of 1. Might that be good enough?" Suppose your left branch has depth 5
; (which you correctly compute), and the right has depth 7, but you compute it
; to be 6, so you don't rebalance it; now your tree is unbalanced, and the
; next depth calculation could potentially be off by 2. By induction, your
; tree may become arbitrarily unbalanced.
; 
; [2] On an efficiency note: My implementation uses a hash table to represent
; each node. This was incredibly convenient for me: x!rt!lf is extremely
; terse, and changing/adding/removing slot names was easy (originally I had
; each node store the depths of the left and right branches; Wikipedia made me
; realize this was redundant). However, it consumes more memory (allocating a
; hash table for each node) and probably more time (doing hash table lookups
; to access each slot) than it really has to.
; 
; One could represent nodes as, e.g., (cons datum (cons left (cons right
; depth))), or (cons (cons left right) (cons datum depth)); define accessors
; as (in the second case) (= lf caar rt cdar dt cadr dp cddr); and replace
; "x!rt!lf" with (lf (rt x)), etc. Or one could use a Racket vector--define
; (datum node) as (vector-ref node 0), (lf node) as (vector-ref node 1), and
; so on. Or one could import Racket structs, and maybe add a "struct?" case
; for "ar-apply" in ac.scm so you can still use the x!rt!lf syntax. All of
; these are likely more efficient than using tables.
; 
; [3] I think the functional approach is easier for the same reason that this:
; 
;   (def insert-sorted (< x xs)
;     (if (or no.xs (< x car.xs))
;       (cons x xs)
;       (cons car.xs (insert-sorted < x cdr.xs))))
; 
; is simpler than this:
; 
;   (def ninsert-sorted (< x xs)
;     (if (or no.xs (< x car.xs))
;       (cons x xs)
;       (do (loop (xs xs)
;             (if (or (no cdr.xs) (< x cadr.xs))
;               (= cdr.xs (cons x cdr.xs))
;               (recur cdr.xs)))
;           xs)))
; 
; [4] I took my code and replaced, e.g., depth:x!lf with (depth x!lf) so this
; works without super-awesome ssyntax. Ad hoc shell scripts are fun.
; 
;   $ pbpaste | sed -E 's/([a-z]+):([a-z]+[.!][a-z]+)/(\1 \2)/g' | pbcopy
