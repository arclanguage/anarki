; Tetris-like game for Arc
; Copyright 2008 Ken Shirriff
; http://arcfn.com

;; Glue between Arc and Scheme
(def quotes (args) (mappend (fn (x) `(',x)) args))

(mac send rest  `(eval-scheme (list 'send ,@rest) ))

(mac instantiate rest  `(eval-scheme (list 'instantiate ,@rest) ))

;; Draw stuff

(def refresh () (send arc-canvas 'refresh))
(def swap-gl-buffers () (send arc-canvas 'swap-gl-buffers))

(= xsize 10)
(= ysize 20)
(= xsize1 (- xsize 1))
(= ysize1 (- ysize 1))

(= colors (obj 0 '(0 0 0 1) 1 '(1 0 0 1) 2 '(0 1 0 1) 3 '(0 0 1 1) 4 '(1 1 0 1) 5 '(0 1 1 1) 6 '(1 1 1 1) 7 '(1 0 1 1)))

(= mode nil)

; Create a shape definition with the given (x y) points.
; Each shape is given in its four rotational positions.
(def createshape (color points0 points1 points2 points3)
      (obj points (list points0 points1 points2 points3) color color))

(= shapes (obj 0 (createshape 1 ;bar
                               '((-1 0) (0 0) (1 0) (2 0))
                               '((0 1) (0 0) (0 -1) (0 -2))
                               '((-1 0) (0 0) (1 0) (2 0))
                               '((0 1) (0 0) (0 -1) (0 -2)))
               1 (createshape 2 ; L
                               '((-1 0) (0 0) (1 0) (-1 -1))
                               '((0 1) (0 0) (0 -1) (-1 1))
                               '((-1 0) (0 0) (1 0) (1 1))
                               '((0 1) (0 0) (0 -1) (1 -1)))
               2 (createshape 3 ; reverse L
                               '((-1 0) (0 0) (1 0) (1 -1))
                               '((0 1) (0 0) (0 -1) (-1 -1))
                               '((-1 0) (0 0) (1 0) (-1 1))
                               '((0 1) (0 0) (0 -1) (1 1)))
               3 (createshape 4 ; T
                               '((-1 0) (0 0) (1 0) (0 -1))
                               '((0 1) (0 0) (0 -1) (-1 0))
                               '((-1 0) (0 0) (1 0) (0 1))
                               '((0 1) (0 0) (0 -1) (1 0)))
               4 (createshape 5 ; square
                               '((0 0) (1 0) (0 -1) (1 -1))
                               '((0 0) (1 0) (0 -1) (1 -1))
                               '((0 0) (1 0) (0 -1) (1 -1))
                               '((0 0) (1 0) (0 -1) (1 -1)))
               5 (createshape 6 ; z
                               '((-1 0) (0 0) (0 -1) (1 -1))
                               '((0 0) (1 0) (1 1) (0 -1))
                               '((-1 0) (0 0) (0 -1) (1 -1))
                               '((0 0) (1 0) (1 1) (0 -1)))
               6 (createshape 7 ; reverse z
                               '((0 0) (1 0) (0 -1) (-1 -1))
                               '((-1 1) (-1 0) (0 0) (0 -1))
                               '((0 0) (1 0) (0 -1) (-1 -1))
                               '((-1 1) (-1 0) (0 0) (0 -1)))))

(= numshapes 7)

(def start-game ()
  (= score 0)
  (= timer-interval 1000)
  (= step 0)
  (= filledrows nil)
  (= rows nil)
  (for y 0 ysize1
    (let row nil
      (for x 0 xsize1
        (push 0 row))
      (push row rows)))
  (= nextshape (shapes (rand numshapes)))
  (newshape)
  (= mode 'run)
  (start-timer timer-interval)
  )

; 7-segment digit definition.  Defines illuminated segments for each number.
(= digits (obj 0 '(0 1 2 4 5 6)
               1 '(2 5)
	       2 '(0 2 3 4 6)
	       3 '(0 2 3 5 6)
	       4 '(1 2 3 5)
	       5 '(0 1 3 5 6)
	       6 '(1 3 4 5 6)
	       7 '(0 2 5)
	       8 '(0 1 2 3 4 5 6)
	       9 '(0 1 2 3 5)))

; x0 y0 x1 y1 for each of the 7 segments
; arranged -0-
;         1   2
;          -3-
;         4   5
;          -6-
(= segments (obj 0 '(0 2 1 2)
                 1 '(0 1 0 2)
		 2 '(1 1 1 2)
		 3 '(0 1 1 1)
		 4 '(0 0 0 1)
		 5 '(1 0 1 1)
		 6 '(0 0 1 0)))


; The square tile that makes up the playing pieces
(def initialize-tile ()
  (= tile (gl-gen-lists 1))
  (gl-new-list tile 'compile)
  (gl-shade-model 'smooth)
  (gl-begin 'quads)
  (gl-normal -1 -1 1)
  (gl-vertex -.5 -.5 0)
  (gl-normal 1 -1 1)
  (gl-vertex .5 -.5 0)
  (gl-normal 1 1 1)
  (gl-vertex .5 .5 0)
  (gl-normal -1 1 1)
  (gl-vertex -.5 .5 0)
  (gl-end)
  (gl-end-list))

; Manage the event timer, which causes the animation
(= timer nil)
(def start-timer (time-interval)
  (if (no timer)
    (= timer (instantiate 'timer% '() (list 'notify-callback (fn () (send arc-canvas 'run))))))
  (send timer 'stop)
  (send timer 'start time-interval))

; Draws a digit from origin, height 2 width 1
(def draw-digit (digit)
  (gl-line-width 1)
  (gl-begin 'lines)
  (each segment (digits digit)
    (let (x0 y0 x1 y1) (segments segment)
      (gl-vertex x0 y0 0)
      (gl-vertex x1 y1 0)))
  (gl-end))


(def draw-score (score)
  (if (is score 0)
        (draw-digit 0)
      (do
        (draw-digit (mod score 10))
	(when (>= score 10)
	  (gl-translate -1.4 0 0)
	  (draw-score (trunc (/ score 10)))))))

(def draw-shape (shape xoff yoff orientation)
   (each (x y) ((shape 'points) orientation)
     (gl-push-matrix)
   (gl-material-v 'front 'ambient-and-diffuse (apply gl-float-vector (colors (shape 'color))))
     (gl-translate (+ x xoff) (+ yoff y) 0)
     (gl-call-list tile)
     (gl-pop-matrix)))

(def draw-rows ()
   (for y 0 ysize1
       (for x 0 xsize1
	   (let val ((rows y) x)
	     (when (isnt val 0)
	       (gl-push-matrix)
	       (if (some y filledrows)
                 (gl-material-v 'front 'ambient-and-diffuse (apply gl-float-vector (colors (mod step 2))))
                 (gl-material-v 'front 'ambient-and-diffuse (apply gl-float-vector (colors val))))
	       (gl-translate x y 0)
               (gl-call-list tile)
	       (gl-pop-matrix)
	       )))))

(def newshape ()
  (= xpos (- (/ xsize 2) 1))
  (= ypos ysize1)
  (= currentshape nextshape)
  (= nextshape (shapes (rand numshapes)))
  (= orientation 0)
  (when (collision? currentshape xpos ypos orientation)
    (saveshape currentshape xpos (+ ypos 1) orientation)
    (= mode 'gameover))
)


; Test for collision
(def collision? (shape xoff yoff orientation)
  (catch
   ; Test if out of bounds
   (each (x y) ((shape 'points) orientation)
     (with (xp (+ x xoff)  yp (+ y yoff))
       (if (or (< xp 0) (< yp 0) (>= xp xsize) (>= yp ysize)) (throw #t))))
   ; Test if collision with stationary blocks
   (each (x y) ((shape 'points) orientation)
     (with (xp (+ x xoff)  yp (+ y yoff))
       (if (isnt ((rows yp) xp) 0) (throw #t))))
   nil))


; Save shape into the permanent grid
(def saveshape (shape xoff yoff orientation)
   (each (x y) ((shape 'points) orientation)
     (with (xp (+ x xoff)  yp (+ y yoff))
      (if (< yp ysize)
       (= ((rows yp) xp) (shape 'color))))))
  
(def rowfilled? (row)
  (is (apply * (map (fn (elt) (if (is elt 0) 0 1)) row)) 1))

; Return new matrix with specified rows removed
; rows-to-remove must be ordered highest-to-lowest
(def remove-rows (matrix rows-to-remove)
  (if rows-to-remove
        (with (result nil blankrow nil)
	  (for y 0 ysize1
	    (if (isnt y (car rows-to-remove))
	          (push (matrix y) result)))
	  (for x 0 xsize1
	    (push 0 blankrow))
	  (push blankrow result) 
	  (remove-rows (rev result) (cdr rows-to-remove)))
	matrix))

; Shape has hit bottom.  Save it in the grid.  Flash the filled rows, or get a new shape.
(def save-and-check ()
  (saveshape currentshape xpos ypos orientation)
  (= score (+ score 5))
  (= filledrows nil)
  (for y 0 ysize1
    (if (rowfilled? (rows y)) (push y filledrows)))
  (if filledrows
    (do 
      (= mode 'flashrows)
      ; make timer fast to flash removed rows
      (start-timer 40)
      (= score (+ score (* (len filledrows) (len filledrows) 10)))
      (= step 0))
    (newshape)))

; Draw border around the playing field
(def draw-border () 
      (gl-begin 'points)
      (gl-vertex -1 -1 0)
      (gl-vertex -1 ysize 0)
      (gl-vertex xsize ysize 0)
      (gl-vertex xsize -1 0)
      (gl-end)
      (gl-begin 'line-loop)
      (gl-vertex -1 -1 0)
      (gl-vertex -1 ysize 0)
      (gl-vertex xsize ysize 0)
      (gl-vertex xsize -1 0)
      (gl-end))

;; GUI handlers

; Key handler
(def ex-on-char (key-event)
 (let char (send key-event 'get-key-code)
  (when (is mode 'run)
    (if (is char 'left)  (if (no (collision? currentshape (- xpos 1) ypos orientation))
	                    (= xpos (- xpos 1)))
        (is char 'up) (if (no (collision? currentshape xpos ypos (mod (+ orientation 1) 4)))
	                    (= orientation (mod (+ orientation 1) 4)))
	(is char 'right) (if (no (collision? currentshape (+ xpos 1) ypos orientation))
	                    (= xpos (+ xpos 1)))
	(is char 'down) (if (no (collision? currentshape xpos (- ypos 1) orientation))
	                   (-- ypos)
			   (save-and-check)))
    (refresh))
 (when (and (is mode 'gameover) (is char #\s))
   (start-game))))

;; Timer callback
(def ex-run ()
  (++ step)
  (when (is mode 'run)  (if (no (collision? currentshape xpos (- ypos 1) orientation))
	                   (-- ypos)
			   (save-and-check)))
  (when (and (is mode 'flashrows) (> step 5))
    ; speed up drop speed
    (= timer-interval (trunc (* timer-interval .9)))
    (start-timer timer-interval)
    (= step 0)
    (= rows (remove-rows rows filledrows))
    (= filledrows nil)
    (newshape)
    (= mode 'run))
  (refresh))

(def ex-on-paint ()
  (gl-clear-color 0 0 0 0)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
      
  ; Set up coordinate system
  ; Origin is in lower left corner of game field.
  ; use ysize+5 for total size to provide room for score, border, etc.
  (gl-push-matrix)
  (gl-scale (/ 1 (+ ysize 5)) (/ 1 (+ ysize 5)) 1)
  (gl-translate 2 2 0)

  (gl-disable 'lighting)

  (gl-color 1 .3 0)
  (gl-push-matrix)
  (gl-translate 16 12 0)
  (draw-score score)
  (gl-pop-matrix)

  ; Red background when game over
  (when (is mode 'gameover)
    (gl-color 1 .2 .2)
    (gl-begin 'quads)
    (gl-vertex -1 -1 0)
    (gl-vertex xsize -1 0)
    (gl-vertex xsize ysize 0)
    (gl-vertex -1 ysize 0)
    (gl-end))

  (gl-color .7 0 0)
  (gl-point-size 7)
  (gl-line-width 7)
  (draw-border)
  (gl-color .7 .3 0)
  (gl-point-size 3)
  (gl-line-width 3)
  (draw-border)


  (gl-enable 'lighting)

  (draw-rows)

  (when (is mode 'run)
    (draw-shape currentshape xpos ypos orientation)
    (draw-shape nextshape (+ xsize 5) ysize1 0))

  (gl-pop-matrix)
  
  (swap-gl-buffers)
  (gl-flush))

(= initialized nil)

(def ex-on-size (width height)
  (when (no initialized)
    (= initialized #t)
    (initialize-tile)
    (start-game)
    (gl-viewport 0 0 width height)
    (gl-matrix-mode 'projection)
    (gl-load-identity)
    (let h (/ height width)
      (gl-frustum 0 1.0 0 h 9.9 10.1))
    (gl-matrix-mode 'modelview)
    (gl-load-identity)
    (gl-translate 0.0 0.0 -10.0))
  
  (gl-light-v 'light0 'position (gl-float-vector 5.0 5.0 10.0 0.0))
  (gl-enable 'cull-face)
  (gl-enable 'lighting)
  (gl-enable 'light0)
  (gl-disable 'depth-test)


  (refresh))

;; Use Scheme to define a arc-canvas% class.  Specify the Arc callback functions.
(eval-scheme `(define arc-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    (define/public (run) (,ex-run))
    (define/override (on-size width height) (with-gl-context (lambda () (,ex-on-size width height))))
    (define/override (on-paint) (with-gl-context ,ex-on-paint))
    (define/override (on-char key-event) (,ex-on-char key-event))
    (super-instantiate () (style '(gl no-autoclear))))))

;; Now create everything
(= frame (instantiate 'frame% '("Arc Game" 'false)))
(= arc-canvas (instantiate 'arc-canvas% (list frame) '(min-width 300) '(min-height 300)))

;; Start everything going
(send frame 'show '#t)
