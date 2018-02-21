;   Create Pd patches in Arc
;   Copyright (C) 2017  Pelle Hjek
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU Affero General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU Affero General Public License for more details.
;
;   You should have received a copy of the GNU Affero General Public License
;   along with this program.  If not, see <https://www.gnu.org/licenses/>.



;                            pd.arc
;                            ======
;
; What is Pd?
;
; Pd is a graphical programming environment for real-time audio synthesis.
; See https://puredata.info
;
;
; Why would you want to create Pd patches in Arc?
;
; * The Pd gui requires faffing about with a mouse
; * Objects aren't first-class in Pd (e.g. objects can create objects)
; * It's lighter than Overtone
;
;
; How does it work?
;
; * Firstly, from the Arc prompt, type (require 'lib/pd.arc)
; * '%' creates Pd objects and messages
; * '->' connects them
; * 'pd' runs the patch in Pd
; * 'pd-reset' reset everything
;
;
; Examples:
;
;   (-> (% loadbang) (% "world") (% print "hello"))
;
;   (-> (list (% osc~ 440) (% osc~ 437)) (% dac~))
;
;
; Todo:
;
; * Pd should automagically reload the patch whenever objects are added
; * ... and hence not eliminate the need to display the Pd gui
; * Open Pd help from the repl
; * More examples and better docs
; * Disconnecting things
;
;
; Maydo:
;
; * Allow subpatches and graph-on-parent
; * Allow symbols and floats
; * Passing objects directly as arguments instead of connecting with `->`
; * Prettier generated patches (e.g. in tree layout)
;
;
; Similar to:
;
; * Overtone (A Clojure interface for SuperCollider)
;   See https://overtone.github.io
;
;
; Dissimilar to:
;
; * Pd-Scheme (A Scheme interpreter embedded in Pd)
;   See https://github.com/etienne-p/Pd_Scheme


; The path to the Pd binary
(= pd-bin      "pd")
(= pd-bin-args " -stderr -send 'pd dsp 1'")

(def pd-reset ()
  "Clears the state of the current Pd patch."
  ; todo: perhaps these globals should be contained within a table
  ;       but it depends on how Pd subpatches will be supported here
  (= pd-objs* (table))
  (= pd-cons* nil)
  (= window   '(600 400))
  (= spacing  '(100 50))
  (= fontsize 20)
  (= id* -1))

(pd-reset)

(declare 'atstrings t)

(deftem 'pd-class
        'id    nil
        'val   nil
        'type  nil
        'props nil
        'x     0
        'y     0)

(deftem 'pd-con
        'from   nil
        'outlet 0
        'to     nil
        'inlet  0)

(def pd-class (pd-type)
  "Create a Pd element w/o a type."
  (fn (val . props)
    (let o
      (inst
        'pd-class
        'id    (++ id*)
        'val   val
        'type  pd-type
        'props props
        'cons  0
        ;evenly space objects
        'x     (mod (* id* (spacing 0)) (window 0))
        'y     (max 0 (* (spacing 1)
                         (roundup (- (/ id* (/ (window 0) (spacing 0))) 0.5)))))
      (= (pd-objs* o!id) o)
      o!id)))

(= pd-obj (pd-class "obj"))
(document pd-class pd-obj props "Creates a Pd object in the current patch.")

(= pd-msg (pd-class "msg"))
(document pd-class pd-msg props "Creates a Pd message in the current patch.")

(= pd-cmt (pd-class "text"))
(document pd-class pd-cmt props "Creates a Pd comment in the current patch.")

(def pd-con (from to (o outlet 0) (o inlet 0))
  "Connects 'outlet(s)' of 'from(s)' to 'inlet(s)' of 'to(s)'."
  (if
    (or (no from) (no to))
      nil
    (acons from)
      (do
        ; if there's more outlets, connect each to the next inlet
        (pd-con (car from) to outlet inlet)
        (pd-con (cdr from) to outlet (inc inlet)))
    (acons to)
      (do
        ; if there's more inlets, connect each to the next outlet
        (pd-con from (car to) outlet inlet)
        (pd-con from (cdr to) (inc outlet) inlet))
    (push
      (ret c
        (inst
          'pd-con
          'from   from
          'outlet outlet
          'to     to
          'inlet  inlet))
      pd-cons*)))

(def -> (from to . then)
  "Connects a series of Pd objects (and return the last one)."
  (zap flat then)
  (pd-con from to)
  (if (car then)
      (cons from
        (-> to (car then) (cdr then)))
      (list from to)))

; todo: is it alright to hijack % ?  (Racket uses % for classes)
(mac % (name . props)
  "Creates a Pd object when given a symbol, or a Pd message when given anything else."
  (if
    (asym name)
      `(pd-obj (quote ,name) ,@props)
    `(apply pd-msg (list ,name ,@props))))

(def pd-c ((o dest))
  "Compiles the current Pd patch to a file and return its path."
  (if (no dest) (= dest (mktemp)))
  (w/stdout (outfile dest)
      (withr
        (pd-obj-name [tostring (apply prs (cons _!val _!props))]
         pd-cnv-c
         ;todo: what are the first two 0's passed to the canvas?
          (fn ()  (prn "#N canvas 0 0 @(window 0) @(window 1) @fontsize;"))
         pd-obj-c
          (fn (o) (prn "#X @o!type @o!x @o!y @(pd-obj-name o);"))
         pd-con-c
          (fn (c) (prn "#X connect @c!from @c!outlet @c!to @c!inlet;")))
        (do
          (pd-cnv-c)
          (walk (map pd-objs* (range 0 id*)) pd-obj-c)
          (walk pd-cons* pd-con-c))))
  dest)

(def pd-exec (file)
  "Open a patch file in Pd."
  ;todo: this is an inelegant way of resetting pd
  (system "killall -9 pd")
  (thread (system "@pd-bin @pd-bin-args @file")))

(def pd ()
  "Run current patch in Pd."
  (pd-exec (pd-c)))

(mac pd-help (pd-obj)
  "Opens the help patch for the given Pd object."
  `(let help-patch
     (+ "/usr/lib/puredata/doc/5.reference/"
        (quote ,pd-obj)
        "-help.pd")
     (aif (file-exists help-patch)
       (pd-exec it))))
