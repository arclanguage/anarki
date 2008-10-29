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
  " Concatenates the tconc cell `vals' onto the tconc cell `var'. By their
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


;; -- Misc algorithms --
(def transpose (g)
  "Graph transposition."
  (with (tg (table)
         al tablist.g)
    (each (k _) al (= tg.k (tconc-new)))
    (each (k v) al (each n car.v (tconc tg.n k)))
    tg))

(def top-sort (g)
  "Topological sorting via postorder depth-first-search. Only makes sense on
DAGs, but neither detect cycles nor loops endlessly on graphs with cycles."
  (withs (vs keys.g
          unvisited memtable.vs)
    (letf (visit (tail v)
            (if (~unvisited v) tail
              (do (= unvisited.v nil) (cons v (foldl visit tail (car g.v))))))
      (foldl visit nil vs))))


;; -- Tarjan's algorithm --
(def tarjan-x (graph)
  "Computes a graph of the strongly connected components of a graph using an
extended version of Tarjan's algorithm."
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
Tarjan's algorithm."
  (keys (tarjan-x g)))
