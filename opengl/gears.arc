;; "gears" demo ported to Arc by Ken Shirriff.
;;
;; This is a version of the venerable "gears" demo for PLT Scheme 200 using
;; Scott Owens' SGL OpenGL bindings.  It was ported from "glxgears.c" 1.3 from
;; XFree86, which had the following notices:
;;
;;     Copyright (C) 1999-2001  Brian Paul   All Rights Reserved.
;;
;;     Permission is hereby granted, free of charge, to any person obtaining a
;;     copy of this software and associated documentation files (the
;;     "Software"), to deal in the Software without restriction, including
;;     without limitation the rights to use, copy, modify, merge, publish,
;;     distribute, sublicense, and/or sell copies of the Software, and to
;;     permit persons to whom the Software is furnished to do so, subject to
;;     the following conditions:
;;
;;     The above copyright notice and this permission notice shall be included
;;     in all copies or substantial portions of the Software.
;;
;;     XFree86: xc/programs/glxgears/glxgears.c,v 1.3 2001/11/03 17:29:20 dawes
;;
;;     This is a port of the infamous "gears" demo to straight GLX (i.e. no
;;     GLUT).  Port by Brian Paul 23 March 2001.
;;
;; Scheme port by Neil W. Van Dyke <neil@neilvandyke.org>, 23 November 2002.
;; Originally called glxgears.ss.  Minor modifications since.
;; See "http://www.neilvandyke.org/opengl-plt/" for more information.

;; Glue between Arc and Scheme
(def quotes (args) (mappend (fn (x) `(',x)) args))

(mac send (obj . rest)
  `(eval-scheme (list 'send ,obj ,@(quotes rest))))

(mac instantiate rest  `(eval-scheme (list 'instantiate ,@rest) ))

;; Constants for gears

(= pi 3.1415926535)
(= step? nil)
(= gear1 nil)
(= view-rotx 20)
(= view-roty 30)
(= view-rotz 0)
(= rotation 0)

(def refresh () (send mygears refresh))
(def swap-gl-buffers () (send mygears swap-gl-buffers))

(def gl-angle-vertex (r theta width)
            (gl-vertex (* r (cos theta)) (* r (sin theta)) width))

;; Build an opengl model of a gear

(def build-gear (inner-radius    ; radius of hole at center
                        outer-radius    ; radius at center of teeth
                        width           ; width of gear
                        teeth           ; number of teeth
                        tooth-depth)    ; depth of tooth
      (withs (r0             inner-radius
             r1             (- outer-radius (/ tooth-depth 2.0))
             r2             (+ outer-radius (/ tooth-depth 2.0))
             da             (/ (* 2.0 pi) teeth 4.0)
             da2            (* da 2)
             da3            (* da 3)
             half-width     (* width 0.5)
             neg-half-width (- half-width))

        (gl-shade-model 'flat)

        (gl-normal 0.0 0.0 1.0)

	;; Draw front face.
        (gl-begin 'quad-strip)
        (for i 0 teeth
          (withs (angle     (/ (* i 2.0 pi) teeth))

            (gl-angle-vertex r0 angle half-width)
            (gl-angle-vertex r1 angle half-width)
            (when (< i teeth)
              (gl-angle-vertex r0 angle half-width)
              (gl-angle-vertex r1 (+ angle da3) half-width))))
        (gl-end)

        ;; Draw front sides of teeth.
        (gl-begin 'quads)
        (for i 0 teeth
          (let angle (/ (* i 2.0 pi) teeth)
            (gl-angle-vertex r1 angle half-width)
            (gl-angle-vertex r2 (+ angle da) half-width)
            (gl-angle-vertex r2 (+ angle da2) half-width)
            (gl-angle-vertex r1 (+ angle da3) half-width)))
        (gl-end)

        (gl-normal 0.0 0.0 -1.0)

        ;; Draw back face.
        (gl-begin 'quad-strip)
        (for i 0 teeth
          (let angle     (/ (* i 2.0 pi) teeth)
            (gl-angle-vertex r1 angle neg-half-width)
            (gl-angle-vertex r0 angle neg-half-width)
            (when (< i teeth)
              (gl-angle-vertex r1 (+ angle da3) neg-half-width)
              (gl-angle-vertex r0 angle neg-half-width))))
        (gl-end)

        ;; Draw back sides of teeth.
        (gl-begin 'quads)
        (for i 0 teeth
          (let angle (/ (* i 2.0 pi) teeth)
            (gl-angle-vertex r1 (+ angle da3) neg-half-width)
            (gl-angle-vertex r2 (+ angle da2) neg-half-width)
            (gl-angle-vertex r2 (+ angle da) neg-half-width)
            (gl-angle-vertex r1 angle neg-half-width)))
        (gl-end)

        ;; Draw outward faces of teeth.
        (gl-begin 'quad-strip)
        (for i 0 teeth
          (withs (angle     (/ (* i 2.0 pi) teeth)
	          cos-angle (cos angle)
		  sin-angle (sin angle))

            (gl-angle-vertex r1 angle half-width)
            (gl-angle-vertex r1 angle neg-half-width)

            (withs (u   (- (* r2 (cos (+ angle da))) (* r1 cos-angle))
                   v   (- (* r2 (sin (+ angle da))) (* r1 sin-angle))
                   len (sqrt (+ (* u u) (* v v))))
              (gl-normal (/ v len) (- (/ u len)) 0.0))

            (gl-angle-vertex r2 (+ angle da) half-width)
            (gl-angle-vertex r2 (+ angle da) neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)
	    (gl-angle-vertex r2 (+ angle da2) half-width)
	    (gl-angle-vertex r2 (+ angle da2) neg-half-width)

            (with (u (- (* r1 (cos (+ angle da3)))
                        (* r2 (cos (+ angle da2))))
                  v (- (* r1 (sin (+ angle da3)))
                        (* r2 (sin (+ angle da2)))))
              (gl-normal v (- u) 0.0))

	    (gl-angle-vertex r1 (+ angle da3) half-width)
	    (gl-angle-vertex r1 (+ angle da3) neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)))

        (gl-angle-vertex r1 0 half-width)
        (gl-angle-vertex r1 0 neg-half-width)
        (gl-end)

        (gl-shade-model 'smooth)

        ;; Draw inside radius cylinder.
        (gl-begin 'quad-strip)
        (for i 0 teeth
          (let angle     (/ (* i 2.0 pi) teeth)
            (gl-normal (- (cos angle)) (- (sin angle)) 0.0)
            (gl-angle-vertex r0 angle neg-half-width)
            (gl-angle-vertex r0 angle half-width)))
        (gl-end)))

;; Callbacks from the GUI

(def ex-run () (= step? t) (refresh))

(def ex-move-left ()
  (= view-roty (+ view-roty 5.0))
  (refresh))

(def ex-move-right ()
  (= view-roty (- view-roty 5.0))
  (refresh))

(def ex-move-up ()
  (= view-rotx (+ view-rotx 5.0))
  (refresh))

(def ex-move-down ()
  (= view-rotx (- view-rotx 5.0))
  (refresh))

; translate and rotate a gear
(def translate-rotate (gear xt yt rotation)
      (gl-push-matrix)
      (gl-translate xt yt 0.0)
      (gl-rotate rotation 0. 0. 1.)
      (gl-call-list gear)
      (gl-pop-matrix))

(def ex-on-paint ()
  (when gear1  
    (when step?
      (= rotation (+ 2.0 rotation))
      
      (gl-clear-color 0.0 0.0 0.0 0.0)
      (gl-clear 'color-buffer-bit 'depth-buffer-bit)
      
      (gl-push-matrix)
      (gl-rotate view-rotx 1.0 0.0 0.0)
      (gl-rotate view-roty 0.0 1.0 0.0)
      (gl-rotate view-rotz 0.0 0.0 1.0)
      
      (translate-rotate gear1 -3 -2 rotation)
      (translate-rotate gear2 3.1 -2 (- (* -2 rotation) 9))
      (translate-rotate gear3 -3.1 4.2 (- (* -2 rotation) 25))
      
      (gl-pop-matrix)
      
      (swap-gl-buffers)
      (gl-flush))
    (when step?
      (= step? nil)
      )))

(def make-gear-list (r g b inner-radius outer-radius width teeth tooth-depth)
  (let gear (gl-gen-lists 1)
    (gl-new-list gear 'compile)
    (gl-material-v 'front
                   'ambient-and-diffuse
                   (gl-float-vector r g b 1.0))
    (build-gear inner-radius outer-radius width teeth tooth-depth)
    (gl-end-list)
    gear))

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
  (gl-enable 'cull-face)
  (gl-enable 'lighting)
  (gl-enable 'light0)
  (gl-enable 'depth-test)
  
  (unless gear1
    (= gear1 (make-gear-list .8 .1 0 1 4 1 20 .7))
    (= gear2 (make-gear-list 0 .8 .2 .5 2 2 10 .7))
    (= gear3 (make-gear-list .2 .2 1 1.3 2 .5 10 .7))
    
    (gl-enable 'normalize))
  (refresh))

;; Use Scheme to define a gears-canvas% class.  Specify the Arc callback functions.
(eval-scheme `(define gears-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    (define/public (run) (,ex-run))
    (define/override (on-size width height) (with-gl-context (lambda () (,ex-on-size width height))))
    (define/override (on-paint) (with-gl-context ,ex-on-paint))
    (super-instantiate () (style '(gl no-autoclear))))))

;; Now create everything
(= frame (instantiate 'frame% '("Gears" 'false)))
(= mygears (instantiate 'gears-canvas% (list frame) '(min-width 300) '(min-height 300)))
(let h (instantiate 'horizontal-panel% (list frame)
                 '(alignment '(center center)) '(stretchable-height 'false))
        (let h2 (instantiate 'horizontal-panel% (list h)
                   '(alignment '(center center)))
          (instantiate 'button% (list "Left" h2 (fn x (ex-move-left))) '(stretchable-width #t))
          (let v (instantiate 'vertical-panel% (list h)
                     '(alignment '(center center)) '(stretchable-width 'false))
            (instantiate 'button% (list "Up" v (fn x (ex-move-up))) '(stretchable-width #t))
            (instantiate 'button% (list "Down" v (fn x (ex-move-down))) '(stretchable-width #t))
          (instantiate 'button% (list "Right" h (fn x (ex-move-right))) '(stretchable-width #t))
)))

;; Start everything going
(send frame show #t)
(instantiate 'timer% '() (list 'notify-callback (fn () (send mygears run))) '(interval 100))
