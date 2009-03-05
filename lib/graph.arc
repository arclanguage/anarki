;; lib/graph.arc - A graph library

;; Authors:
;; - Michael Arntzenius <daekharel@gmail.com>

;; License: Do what you want, but it's not my fault!

;; Feel free to make additions to this file and push them to anarki, as long as
;; you don't mind the licensing (above). Keep it to graph algorithms. Don't
;; change stuff other people added. Add yourself to the authors list if you
;; want.

;; ---- DESCRIPTION ----

;; Unless otherwise noted, graphs are represented in adjacency list form, as
;; tables from nodes to tconc cells of neighbors (see lib/tconc.arc).

;; While using tconc cells might seem like premature optimization, using
;; something other than plain old lists is in fact necessary. Otherwise, a node
;; with no outgoing edges would be "mapped to nil", and hence _not present at
;; all_, in the graph's table representation - a very undesirable situation.

;; Also note that nodes may be any data type, but if they are mutated, this
;; invalidates any graphs involving them (if a key in a table mutates, its value
;; can no longer be found).

(require "lib/util.arc")
(require "lib/tconc.arc")

;; TODO: ask AmkG whether we can add this to lib/tconc.arc.
(def jconc (var vals)
  "Concatenates the tconc cell `vals' onto the tconc cell `var'. By their
nature, tconc cell operations are destructive, and 'jconc is no exception: it
mutates `var' and reuses the storage space for `vals'. Moreover, modifying
`vals' after calling 'jconc will break the abstraction."
  (awhen car.vals
    (if car.var (= (cdr (cdr var)) it) (= (car var) it))
    (= (cdr var) cdr.vals))
  var)

(def lconc-new (vals)
  (lconc (tconc-new) vals))

(def tconc-list vals
  (lconc-new vals))

(def map1-conc (fun vals)
  (let out (tconc-new)
    (each e car.vals (tconc out (fun e)))
    out))


;; -- Convenience --
(def alist-graph (l)
  (listtab (mapeach (k v) l (list k (lconc-new v)))))

(def list-graph (l)
  (listtab (mapeach (k . v) l (list k (lconc-new v)))))

(def graph-print (g (o sort-comparison nil) (o printer pr))
  "Pretty-prints a graph."
  (let sortfn (if sort-comparison [sort sort-comparison _] idfn)
    (each v (sortfn keys.g)
      (printer v)
      (pr ": ")
      (awhen (car g.v)
        (zap sortfn it)
        (printer car.it)
        (each n cdr.it (pr " ") (printer n)))
      (prn))))

(def add-implied-leaves (g)
  "Adds vertices which are the destination of an edge but are not themselves
present in the graph. The added vertices will have no outgoing edges."
  (each v keys.g
    (each n (car g.v)
      (unless g.n
        (= g.n (tconc-new))))))


;; -- Misc algorithms --
(def transpose (g)
  "Graph transposition."
  (let tg (table)
    (ontable k _ g (= tg.k (tconc-new)))
    (ontable k v g (each n car.v (tconc tg.n k)))
    tg))

(def acyclic (g)
  "Returns nil if the graph contains a cycle."
  (catch
    (let info (table)
      (letf (visit (v)
              (case info.v
                nil (do
                      (= info.v 'current)
                      (each n (car g.v) visit.n)
                      (= info.v 'done))
                current throw.nil))
        (ontable v _ g
          (visit v))))
    t))

(def out-degrees (g)
  (let degs (table)
    (ontable k v g
      (= degs.k (len:car v)))
    degs))

(def in-degrees (g)
  (let degs (table)
    (ontable k _ g (= degs.k 0))
    (ontable _ ns g
      (each n car.ns (++ degs.n)))
    degs))

(def graph-roots (g)
  "Finds vertices with no incoming edges.
  See also: [[graph-leaves]]"
  (let roots (table)
    (ontable v _ g (assert roots.v))
    (ontable _ ns g (each n car.ns (wipe roots.n)))
    keys.roots))

(def graph-leaves (g)
  "Finds vertices with no outgoing edges.
  See also: [[graph-roots]]"
  (let vs nil
    (ontable v ns g
      (when (no car.ns) (push v vs)))
    vs))


;; -- Topological sorting --
(def top-sort (graph (o roots (keys graph))
                (o fail (fn () (err 'top-sort "graph contains cycle"))))
  "Topological sorting via postorder depth-first-search, considering only nodes
reachable from 'roots, which defaults to all vertices in the graph. Will call
the 'fail parameter if a cycle is detected, which defaults to raising an error.
  See also: [[top-sort-fast]] [[acyclic]]"
  (let info (table)
    (foldl
      (afn (tail v)
        (case info.v
          done tail
          nil (do
                (= info.v 'current)
                (let result (cons v (foldl self tail (car graph.v)))
                  (= info.v 'done)
                  result))
          current (fail)))
      nil roots)))

(def top-sort-fast (graph (o roots (keys graph)))
  "Topological sorting via postorder depth-first-search, considering only nodes
reachable from 'roots, which defaults to all vertices in the graph. Neither
detects cycles nor loops endlessly on graphs with cycles.
  See also: [[top-sort]]"
  (let visited (table)
    (foldl (afn (tail v)
             (if visited.v tail
               (do
                 (= visited.v t)
                 (cons v (foldl self tail (car graph.v))))))
      nil roots)))

;; ;; reference implementation for top-sort-fast
;; ;; TODO: compare speed to current 'top-sort-fast
;; (def top-sort-fast (graph (o roots (keys graph)))
;;   (rev:post-orderf:spanning-forest graph roots))


;; -- Tree algorithms --

;; N-ary trees are represented as a cons of their value and a list of their
;; children. Forests are lists of trees.

(def spanning-forest (graph (o roots (keys graph)))
  "Finds a spanning forest of the part of the graph reachable from 'roots, which
defaults to all vertices in the graph, obtained from depth-first-search starting
at those vertices in order."
  (let visited (table)
    (trues (afn (v) (when (~visited v)
                      (assert visited.v)
                      (cons v (trues self (car graph.v)))))
      roots)))

(def pre-order (tree (o tail)) (cons car.tree (foldr pre-order tail cdr.tree)))
(def post-order (tree (o tail)) (foldr post-order (cons car.tree tail) cdr.tree))

(def pre-orderf (forest (o tail)) (foldr pre-order tail forest))
(def post-orderf (forest (o tail)) (foldr post-order tail forest))


;; -- Tarjan's algorithm --
(def tarjan-x (graph)
  "Computes a graph of the strongly connected components of a graph using an
extended version of Tarjan's algorithm.
  See also: [[tarjan]]"
  (with (info (table)
         scc (table)
         scc-graph (table)
         index 0)
    (withf ((make-scc (nodes succs)
              (each n car.nodes (= scc.n nodes))
              (= (scc-graph car.nodes) (map1-conc car succs))
              nodes)
            (visit (v)
              (with (nodes (tconc-new)
                     succs (tconc-new)
                     v-info (cons index index))
                ;; Add our info to the "stack" and increment the index
                (= info.v v-info)
                (++ index)

                ;; visit all neighbors
                (each n (car graph.v)
                  (aif
                    ;; the neighbor has been visited and assigned an SCC. We add
                    ;; it to our list of successor SCCs
                    scc.n (tconc succs it)

                    ;; The neighbor is on the stack. If it has a lower lowlink
                    ;; that us, this becomes our lowlink.
                    info.n (= cdr.v-info (min cdr.v-info cdr.it))

                    ;; The neighbor is unvisited; recurse.
                    (let (sub-nodes sub-succs) (visit n)
                      (jconc nodes sub-nodes)
                      (jconc succs sub-succs)
                      (= cdr.v-info (min cdr.v-info (cdr info.n))))))

                ;; add v to the nodes in the SCC we are building.
                (tconc nodes v)

                ;; v is root of an SCC if its index equals its lowlink.
                (if (is car.v-info cdr.v-info)
                  ;; we are the root, create a new SCC and return it as a single
                  ;; successor to our parent.
                  (list (tconc-new) (tconc-list (make-scc nodes succs)))
                  ;; we are not the root, return the SCC fragment for our parent
                  ;; to complete.
                  (list nodes succs)))))
      (ontable v _ graph
        (unless info.v
          (visit v))))
    scc-graph))

(def tarjan (g)
  "Computes a list of the strongly connected components of a graph using
Tarjan's algorithm.
  See also: [[tarjan-x]]"
  (keys (tarjan-x g)))
