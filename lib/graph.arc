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
(def list-graph (l)
  (listtab (mapeach (k v) l (list k (lconc-new v)))))

(def mlist-graph (l)                    ;m for multi
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
    (withs (vs keys.g
            info memtable.vs)
      (map1 (afn (v)
              (case info.v
                t (do (= info.v 'current) (map1 self (car g.v)) (= info.v nil))
                current (throw nil)))
        vs)
      t)))

(def top-sort (graph (o roots (keys graph))
                (o fail (fn () (err 'top-sort "graph contains cycle"))))
  "Topological sorting via postorder depth-first-search, considering only nodes
reachable from 'roots, which defaults to all vertices in the graph. Will call
the 'fail parameter if a cycle is detected, which defaults to raising an error.
  See also: [[top-sort-fast]]"
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


;; -- Tarjan's algorithm --
(def tarjan-x (graph)
  "Computes a graph of the strongly connected components of a graph using an
extended version of Tarjan's algorithm.
  See also: [[tarjan]]"
  (with (vs keys.g
         info (table)
         scc (table)
         scc-graph (table)
         index 0)
    (let unvisited memtable.vs
      (withf ((make-scc (nodes succs)
                (each n car.nodes (= scc.n nodes))
                (= (scc-graph car.nodes) (map1-conc car succs))
                nodes)
              (visit (v)
                (= unvisited.v nil)
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
        (each v vs
          (when unvisited.v
            (visit v)))
        scc-graph))))

(def tarjan (g)
  "Computes a list of the strongly connected components of a graph using
Tarjan's algorithm.
  See also: [[tarjan-x]]"
  (keys (tarjan-x g)))
