;; Glue between Arc and Scheme
;; These macros provide cleaner access to Scheme's "send" and "instantiate".
(def quotes (args) (mappend (fn (x) `(',x)) args))

(mac send (obj . rest)
  `(eval-scheme (list 'send ,obj ,@(quotes rest))))

(mac instantiate rest  `(eval-scheme (list 'instantiate ,@rest) ))

;; Definition of the Arch

; The arch is specified by the following:
; r: radius of the curved part.  Also half-width of opening
; h1: height from the base to the bottom of the curve
; h2: height above the top of the curve
; w: half-width of the whole arch
; z: half-depth.
; The arch is centered with 0, 0 at the center of the curve
(def arch (r h1 h2 w z)
    (withs (n 10
            fan (archfan r h1 h2 w n)
            revfan (rev fan)
	    negz (- z))
      ; front rhs
      (gl-normal 0 0 1)
      (gl-begin 'triangle-fan)
        (gl-vertex w (+ r h2) z)
        (each (x y) fan
          (prn x " " y " " z)
          (gl-vertex x y z))
      (gl-end)
      ; front lhs
      (gl-begin 'triangle-fan)
        (gl-vertex (- w) (+ r h2) z)
        (each (x y) revfan
          (prn x " " y " " z)
          (gl-vertex (- x) y z))
      (gl-end)
      ; back rhs
      (gl-normal 0 0 -1)
      (gl-begin 'triangle-fan)
        (gl-vertex w (+ r h2) negz)
        (each (x y) revfan
          (prn x " " y " " negz)
          (gl-vertex x y negz))
      (gl-end)
      ; back lhs
      (gl-begin 'triangle-fan)
        (gl-vertex (- w) (+ r h2) negz)
        (each (x y) fan
          (prn x " " y " " negz)
          (gl-vertex (- x) y negz))
      (gl-end)
      (gl-begin 'quads)
        ; top
        (gl-normal 0 1 0)
        (gl-vertex (- w) (+ r h2) z)
        (gl-vertex w (+ r h2) z)
        (gl-vertex w (+ r h2) negz)
        (gl-vertex (- w) (+ r h2) negz)
        ; right
        (gl-normal 1 0 0)
        (gl-vertex w (+ r h2) z)
        (gl-vertex w (- h1) z)
        (gl-vertex w (- h1) negz)
        (gl-vertex w (+ r h2) negz)
        ; left
        (gl-normal -1 0 0)
        (gl-vertex (- w) (+ r h2) z)
        (gl-vertex (- w) (+ r h2) negz)
        (gl-vertex (- w) (- h1) negz)
        (gl-vertex (- w) (- h1) z)
        ; inside left
        (gl-normal 1 0 0)
        (gl-vertex (- r) (- h1) z)
        (gl-vertex (- r) 0 z)
        (gl-vertex (- r) 0 negz)
        (gl-vertex (- r) (- h1) negz)
        ; inside right
        (gl-normal -1 0 0)
        (gl-vertex r (- h1) negz)
        (gl-vertex r 0 negz)
        (gl-vertex r 0 z)
        (gl-vertex r (- h1) z)
        ; bottom left
        (gl-normal 0 -1 0)
        (gl-vertex (- r) (- h1) z)
        (gl-vertex (- w) (- h1) z)
        (gl-vertex (- w) (- h1) negz)
        (gl-vertex (- r) (- h1) negz)
        ; bottom right
        (gl-normal 0 -1 0)
        (gl-vertex w (- h1) z)
        (gl-vertex r (- h1) z)
        (gl-vertex r (- h1) negz)
        (gl-vertex w (- h1) negz)
        ; inside arch
        (for i 0 (+ n n -1)
          (withs (angle (* (/ 3.1415926 n 2) i)
                  angle2 (* (/ 3.1415926 n 2) (+ i 1)))
	    (gl-normal (* r (cos angle)) (* r -1 (sin angle)) 0)
	    (gl-vertex (* r -1 (cos angle)) (* r (sin angle)) z)
	    (gl-vertex (* r -1 (cos angle)) (* r (sin angle)) negz)
	    (gl-normal (* r (cos angle2)) (* r -1 (sin angle2)) 0)
	    (gl-vertex (* r -1 (cos angle2)) (* r (sin angle2)) negz)
	    (gl-vertex (* r -1 (cos angle2)) (* r (sin angle2)) z)))
      (gl-end)
      ))


; Returns the triangle fan (x y) pairs for the right hand part of the arch.
(def archfan (r h1 h2 w n)
  (rev (accum collect
    (collect (list 0 (+ r h2)))
    (collect (list 0 r))
    (for i 0 n
      (let angle (* (/ 3.1415926 n 2) i)
        (collect (list (* r (sin angle)) (* r (cos angle))))))
    (collect (list r (- h1)))
    (collect (list w (- h1))))))

; rotation angles and scale for displaying the arch
(= rx 2)
(= ry 3)
(= rz 4)
(= s 2)

; Helpers

(def refresh () (send arc-canvas refresh))
(def swap-gl-buffers () (send arc-canvas swap-gl-buffers))

;; Callbacks from the GUI

(def ex-run ()
  (= rx 0)
  (= ry (+ (/ 3.1415926535 3) ry))
  (= rz 0)
  (refresh))

(def ex-on-paint ()
  (when box  
      (gl-clear-color 0 0 0 0)
      (gl-clear 'color-buffer-bit 'depth-buffer-bit)
      (gl-push-matrix)
      (gl-rotate rx 1 0 0)
      (gl-rotate rz 0 0 1)
      (gl-rotate ry 0 1 0)
      (gl-scale s s s)
      
      (gl-call-list box)
      (gl-pop-matrix)
      
      (swap-gl-buffers)
      (gl-flush))
      )

(def ex-on-size (width height)
  (gl-viewport 0 0 width height)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (let h (/ height width)
    (gl-frustum -1.0 1.0 (- h) h 5.0 60.0))
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  (gl-translate 0.0 0.0 -40.0)
  
  (gl-light-v 'light0 'position (gl-float-vector 5.0 5.0 10.0 0.0))
  ;(gl-enable 'cull-face)
  (gl-enable 'lighting)
  (gl-enable 'light0)
  (gl-enable 'depth-test)

  (= box (gl-gen-lists 1))
  (gl-new-list box 'compile)
  (gl-material-v 'front 'ambient-and-diffuse (gl-float-vector .87 .443 .03 1))
  (gl-shade-model 'smooth)
  (arch 1 2 1 2 .5)
  (gl-end-list)
  (gl-enable 'normalize)
  (refresh))
;; Now create everything
(= frame (instantiate 'frame% '("OpenGL Demo" 'false)))

;; Use Scheme to define a arc-canvas% class.  Specify the Arc callback functions.
(eval-scheme `(define arc-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    (define/public (run) (,ex-run))
    (define/override (on-size width height) (with-gl-context (lambda () (,ex-on-size width height))))
    (define/override (on-paint) (with-gl-context ,ex-on-paint))
    (super-instantiate () (style '(gl no-autoclear))))))

;; Now create everything
(= frame (instantiate 'frame% '("OpenGL Demo" 'false)))
(= arc-canvas (instantiate 'arc-canvas% (list frame) '(min-width 300) '(min-height 300)))

;; Show the graphics
(send frame show #t)

; Macro to start animation
(mac motion ()
'(instantiate 'timer% '() (list 'notify-callback (fn () (send arc-canvas run))) '(interval 1))
)

