;******************************************************************************
;* lmr.arc                                                                    *
;* The Light Makes Right ray tracer.                                          *
;*                                                                            *
;* Ajay Kapal                                                                 *
;* 2008                                                                       *
;******************************************************************************

;*********************************************
;* Operations on vectors of arbitrary length *
;*********************************************

(def vmag (vec1)
  " Returns the magnitude of a vector."
     (sqrt (apply + (map [* _ _] vec1))))

(def vdot (vec1 vec2)
  " Returns the dot product of two vectors."
     (apply + (map * vec1 vec2)))

(def vadd (vec1 vec2)
  " Returns the result of adding two vectors."
     (map + vec1 vec2))

(def vsub (vec1 vec2)
  " Returns the result of subtracting two vectors."
     (map - vec1 vec2))

(def vscal-mul (scal1 vec1)
  " Returns the result of multiplying a vector by a scalar."
     (map [* _ scal1] vec1))

(def vneg (vec1)
  "Returns negated vector."
  (map [* _ -1] vec1))

(def vcomb (scal1 vec1 scal2 vec2)
  " Linear combination:  returns scal1 * vec1 + scal2 * vec2."
     (map + (map [* _ scal1] vec1) (map [* _ scal2] vec2)))

(def vadd-scal (scal1 vec1 vec2)
  " Add scalar multiple:  returns scal1 * vec1 + vec2"
     (map + (map [* _ scal1] vec1) vec2))

(def vnorm (vec1)
  " Returns a normalized vector."
     (map [/ _ (vmag vec1)] vec1))

(def ray-point (t vec_origin vec_dir)
  " Given a ray and the parameter t, return the point on the ray."
     (vadd-scal t vec_dir vec_origin))

(def cross3 (vec1 vec2)
  " Cross product of two vectors"
  (list (- (* (vec1 1) (vec2 2)) (* (vec1 2) (vec2 1)))
	(- (* (vec1 2) (vec2 0)) (* (vec1 0) (vec2 2)))
	(- (* (vec1 0) (vec2 1)) (* (vec1 1) (vec2 0)))))

;********************
;* Scene Management *
;********************

(def read-scene (file)
  " Reads a .lmr file and returns a scene."
     (w/infile f file
     (read f)))

(def scene-name (scene)
  " The scene-name is extracted from a  scene (.lmr) file.  Returns a string."
     (each el scene
	   (if (isa el 'cons)
		 (when (is (car el) 'name)
		     (= rv (car (cdr el))))))
     rv)

(def get-scene-objs (id scene)
  " A workhorse function to return a list of all objects of type 'id' within 
    a scene.  
    For example:  (get-scene-objs \"shape\" (read-scene (\"demo.lmr\")))"
  (let obj_lst ()
    (each el scene
	  (if (isa el 'cons)
	      (when (is (car el) (sym id))
		(push (cdr el) obj_lst)))) 
    obj_lst))
		       

;*************
;* Constants *
;*************

(def init-globals (scene-file)

; general constants
  (set entering 1)
  (set exiting 0)
  (set maxlevel 5)
  (set minweight 0.01)
  (set rayeps 1e-7)

  ; things we want to often access from the scene file.
  (set scene (read-scene scene-file))
  (set bg_col (alref scene 'bg_color))
  (set shapes (get-scene-objs "shape" scene))
  (set eye (car (get-scene-objs "camera" scene))) ; 1 camera per scene
  (set eye_loc (alref eye 'loc))
  (set screen (flat (get-scene-objs "screen" scene)))
  (set lights (get-scene-objs "light" scene))
  (set res (flat (get-scene-objs "img_res" scene))))


;*************************
;* Intersection routines *
;*************************

; function definitions for spheres.
(= sphere (table))
(= (sphere 'intersect) (fn (ray shape)
  (with (v nil b nil disc 0 rad (alref shape 'rad) t1 0 t2 0 hits nil)
	; distance between ray origin and center of sphere
	(= v (vsub (alref shape 'loc) (car ray)))
	; calc ray distance closest to center
	(= b (vdot v (ray 1)))
	(= disc (+ (- (* b b) (vdot v v)) (* rad rad)))
	(if (<= disc 0)
	    nil
	    (do 
		(zap sqrt disc)
		(= t2 (+ b disc))
		(if (<= t2 rayeps)
		    nil
		    (do 
		      (= t1 (- b disc))
		      (if (> t1 rayeps)
			  (= hits (cons `(,t1 entering ,nil ,shape) hits)))
		      (= hits (cons `(,t2 exiting ,(alref shape 'medium) ,shape) hits)))))))))

(= (sphere 'normal) (fn (pt shape)
  (vnorm (vsub pt (alref shape 'loc)))))

; function definitions for planes.
(= plane (table))
(= (plane 'intersect) (fn (ray shape)
  (with (hits nil)
    (= N (normal nil shape))
    (= N_dot_Rd (vdot N (ray 1)))
    (if (< N_dot_Rd 0) ; plane normal points to our ray
	(do 
	  (= D (alref shape 'dfc))
	  (= t1 (/ (- (+ (vdot N (ray 0)) D)) N_dot_Rd))
	  (if (> t1 0) ; ray is pointing away from plane
	      (= hits (cons `(,t1 entering ,nil ,shape) hits))))
	nil))))

(= (plane 'normal) (fn (pt shape) ;ignore pt for planes.
  (vnorm (alref shape 'norm))))

(= shape-types (table))
(= (shape-types 'sphere) sphere)
(= (shape-types 'plane) plane)

(def intersect (ray shape)
  " Intersection of a ray and a shape.
    The ray is specified as ((ox oy oz) (dx dy dz)) where o is the origin
    and d is the direction.
    The shape is read in from the scene file, see the definition there.
    If a shape is intersected, the routine should return a list containing
    the value of ray parameter t, the symbol 'entering or 'exiting to indicate
    if the ray travels thru the obj, or hits its surface, and finally the 
    shape that is hit.
    If there is no intersection, nil is returned."
  (withs (sh_type (alref shape 'type) class (shape-types sh_type) function (class 'intersect)) (function ray shape)))

(def normal (pt shape)
  " Calculate the normal of a point on a shape.
    pt is a point of form (px py pz).
    shape is read in from the scene file, see the definition there."
  (withs (sh_type (alref shape 'type) class (shape-types sh_type) function (class 'normal)) (function pt shape)))

(def p_to_w (x_pix y_pix z_pix)
  " Convert from pixel to world coordinates.
    x_pix, y_pix, z_pix - 3d coordinate in pixel space
    screen_coord - The world coordinates, as defined by the 'screen' 
                   declaration in the scene file.
    res - The output pixel resolution, as defined by the 'img_res'
          declaration in the scene file."
  (with (xw_min (screen 0) xw_max (screen 1) yw_min (screen 2) yw_max (screen 3))
	 ( list ( + ( * (/ x_pix (res 0)) (- xw_max xw_min)) xw_min)
         ( - yw_max ( * (/ y_pix (res 1)) (- yw_max yw_min))) 0)
	 ))

(def gen-ray-dir (scr_pt eye)
  " Given a point on our screen which is defined in world coordinates 
    as specified by the 'screen' directive in the scene file, and given
    the camera/eye, generate a normalized ray direction vector"
  (vnorm (vsub scr_pt eye)))

(def spec-dir (I N)
  " Return the specular direction given the incident and surface normal.
    All vectors are unit length."
  (vnorm (vadd-scal (* -2 (vdot I N)) N I)))

(def trans-dir (ir_med1 ir_med2 I N)
  " Use Snell's law ( n1*sin(theta1) = n2*(sin(theta2)) ) to find 
    the transmission dir from medium 1 to medium 2.
    I - incident direction
    N - normal
    ir_med1 - index of refraction of medium 1
    ir_med2 - index of refraction of medium 2
    returns the transmission dir, or nil"
  (if ir_med1
      (= n1 ir_med1)
      (= n1 1))
  (if ir_med2
      (= n2 ir_med2)
      (= n2 1))

  (= rel_ir (/ n1 n2))
  (= cos1 (- (vdot I N)))
  (= cos2 (- 1 (* rel_ir rel_ir (- 1 (* cos1 cos1)))))

  (if (>= cos2 0)
      (vcomb rel_ir I (- (* rel_ir cos1) (sqrt cos2)) N)
      nil))

(def render (scene_file)
  " Render a scene file, writing the output to a .bmp whose name
    is derived from the 'name' declaration in the scene file."
  (with (ray nil scanline nil img_data nil)
    (prn)
    (prn "Light Makes Right renderer.")
    (prn "by Ajay Kapal.")
    (prn  "March 2008.")
    (prn)
    (pr "Start Time:  ")
    (prn (system "date"))

    (init-globals scene_file)

    ; main loop
    (for y 0 (- (res 1) 1)	 
	 (= scanline nil)
	 (for x 0 (- (res 0) 1)
	      ; 4x AA
	      (= col '(0 0 0))
	      (for sub_x 0 1
		   (for sub_y 0 1
			;screen is a plane on z=0.
			(= ray (list eye_loc (gen-ray-dir (p_to_w (+ x (* 0.5 sub_x)) (+ y (* 0.5 sub_y)) 0) eye_loc)))

			(= col (vadd (trace 0 1 ray) col))))
	      (= col (map [round (/ _ 4)] col))
	      (= scanline (cons col scanline)))
	 (push (pad-bmp-scanline res (copy (rev scanline))) img_data)
	 (pr (+ (string y) ".")))
    (prn)
    (pr "End Time:  ")
    (prn (system "date"))
    (= file_name  (+ (car (flat (get-scene-objs "name" scene))) ".bmp"))
    (save-image-data file_name (res 0) (res 1) img_data)))

(def trace (level weight ray)
  " Traces a ray in world space thru the scene.  Returns the
    calculated color, or the background color if no shape was intersected."
  (with (surface_pt nil surface_norm nil intersections nil next_closest nil)
    (= intersections (gen-intersections ray))
    (if intersections
	(do 
	  (= closest (intersections 0))
	  (= surface_pt (ray-point (closest 0) (ray 0) (ray 1)))
	  (= surface_norm (normal surface_pt (last closest)))
	  (if (> (vdot (ray 1) surface_norm) 0)
	      (zap vneg surface_norm))
	  (= ints (list closest))
	  (if (> (len intersections) 1)
	      (= next_closest (intersections 1))
	      (do
		(if (is (alref (last closest) 'name) 
			(alref (last next_closest) 'name))
		    (= ints (list closest next_closest)))))
	  (shade level weight surface_pt surface_norm (ray 1) ints))
	bg_col)))

(def shade (level weight P N I intersections) 
  " Shade a surface point.  Stop when level > maxlevel or when 
    weight < minweight.
    lights - list of lights in the scene  
    P - point on a surface
    N - normal at pt
    I - incident ray direction
    intersections - a list of intersections (as defined by what the intersect
                    routine returns).
    returns:  color"
  (with (intensity 0 diffuse 0 ambient 0 specular 0 sh nil light_type nil col nil)
    (= sh (last (car intersections)))
    (each lt lights
	  (if (is (alref lt 'type) 'point)
	      (do
		(= L (vsub (alref lt 'loc) P))
		(if (no (shadow (list P (vnorm L))
				(vmag (vsub P (alref lt 'loc)))))
		    (do
		      ; diffuse
		      (= N_dot_L (vdot N L))
		      (if (> N_dot_L 0)
			  (= diffuse (+ (attenuate (* (alref sh 'kd) (alref lt 'intensity) N_dot_L) P lt) diffuse)))
		      ; specular
		      (= V (vnorm (vsub eye_loc P)))
		      (= R (vnorm (vsub (vscal-mul (* 2 N_dot_L) N) L)))
		      (= R_dot_V (vdot R V))
		      (if (> R_dot_V 0)
			  (= specular (+ (attenuate (* (alref sh 'ks) (alref lt 'intensity) (expt R_dot_V (alref sh 'shininess))) P lt) specular))))))
	      (= amb_lt lt))) ; only one ambient light in the scene

    (= ambient (* (alref amb_lt 'ka) (alref amb_lt 'intensity)))
    (= intensity (+ ambient diffuse specular))
    (= col (vscal-mul intensity (rev (shape-color sh P))))

    (if (< (+ level 1) maxlevel)
	(with (ks_weight (* (alref sh 'ks) weight) kt_weight (* (alref sh 'kt)) r_ray (list (copy P) '(0 0 0)) r_col nil)
	  ; recursively generate specular reflection rays
	  (if (> ks_weight minweight)
	      (do 
		(= (r_ray 1) (spec-dir I N))
		(= r_col (trace (+ level 1) ks_weight r_ray))
		(if r_col
		    (= col (vadd-scal (alref sh 'ks) r_col col)))))

	  ; recursively generate transmission rays
	  (if (> kt_weight minweight)
	      (do 
		; retrieve entering and exiting media
		(if (is ((car intersections) 1) 'entering)
		    (do 
		      (= m_enter nil)
		      (= m_exit (alref (last (car intersections)) 'medium)))
		    (do
		      (= m_enter (alref (last (car intersections)) 'medium))
		      (= m_exit nil)))
		(= (r_ray 1) (trans-dir m_enter m_exit I N))
		(if (r_ray 1)
		    (= t_col (trace (+ level 1) kt_weight r_ray)))
		(if t_col
		    (= col (vadd-scal (alref sh 'kt) t_col col)))))))
	(= col (map [clamp _ 255] col))))

;****************************
;* Texture related routines *
;****************************

(def checkerboard (P)
  "Generate a checkerboard pattern."
  (mod (+ (mod (round (abs (P 0))) 2)
	  (mod (round (abs (P 1))) 2)
	  (mod (round (abs (P 2))) 2)) 2))


(def shape-color (sh P)
  "Determine the shape color.  If a plane has a texture directive, 
   map a checkerboard onto it."
  (if (is (alref sh 'type) 'plane)
      (do 
	(= tex (alref sh 'texture))
	(if tex
	    (if (is (checkerboard P) 0)
		(alref tex 'color0)	
		(alref tex 'color1))))
;	(alref sh 'color))
      (alref sh 'color)))

(def shadow (ray dist_P_to_L)
  " Determine if a light is visible from point P.  Return true if the point 
    is in a shadow."
  (with (intersections nil)
    (= intersections (gen-intersections ray))
    (if (or (no intersections) (> (caar intersections)  (- dist_P_to_L rayeps)))
	nil
	t)))

(def gen-intersections (ray)
  " Encapsulates common intersection code.  Returns a list of 
    intersections, sorted by ascending distance."
  (= intersections nil)
  (each sh shapes
	(= sh_intersections (intersect ray sh))
	(if sh_intersections
	    (map [push _ intersections] sh_intersections)))
  (if intersections
      (sort (fn (x y) (< (car x) (car y))) intersections)))

(def attenuate (component P light)
  " Used to attenuate the diffuse and specular contributions of our
    point lights, according to the formula
    att_factor = min(1, 1/(c1 + c2di + c3di^2)) where
                 di = dist. from intersection point P to ith point light src
                 c1, c2, c3 are light attenuation coefficients, set to
                 (0, 0, 0.08) respectively.

    component - the diffuse or specular component for the given light
    P - point of intersection
    light - point light
    Returns the attenuated component."
  (with (c1 0 c2 0 c3 0.08 d (vmag (vsub (alref light 'loc) P)) att 0)
    (= att (/ 1 (+ c1 (* c2 d) (* c3 (* d d)))))
    (if (> att 1)
	(= att 1))
    (* component att)))
;*************************
;* BMP-related functions *
;*************************

(def build-bmp-hdr (width height)
  " Return a list of bytes to represent a properly formatted BMP header.
    The file size is calculated based on the width and height, plus appropriate
    padding of each scanline to a multiple of 4 bytes."
     (with (bmp-hdr-size 14 bmp-info-hdr-size 40 pad_bytes (* (mod (* width 3) 4) height))
	   (flat (list ;bitmapfileheader follows:
	         (gen-word 19778) ; ascii for 'BM' 
		 (gen-long (+ bmp-hdr-size bmp-info-hdr-size (* width height 3) pad_bytes))
		 (gen-word 0)
		 (gen-word 0)
		 (gen-long (+ bmp-hdr-size bmp-info-hdr-size))
		 ; bitmapinfoheader follows:
		 (gen-long 40)
		 (gen-long width)
		 (gen-long height)
		 (gen-word 1)
		 (gen-word 24)
		 (gen-long 0)
		 (gen-long 0)
		 (gen-long 0)
		 (gen-long 0)
		 (gen-long 0)
		 (gen-long 0)))))

(def pad-bmp-scanline (res scanline)
  " Pad out each scanline we generate to a multiple of 4 bytes.  Flatten the
    resulting list into a stream of bytes.
    res - (x_res y_res)
    scanline - a list of rgb triplets"
  (with (num_pad_bytes (mod (* (res 0) 3) 4))
	(repeat num_pad_bytes (= scanline (list scanline 0))))
  (flat scanline))

(def save-image-data (name width height data)
  " Write data to a BMP file.
    name - name of BMP file.
    data - list of scanlines.  Each scanline must already be padded
           appropriately for BMP output (ie, must be a multiple of 4 bytes).
    NOTE:  This routine assumes each pixel in a scanline is a BGR triplet."
  (prn "Saving image data.")
  (with (header (build-bmp-hdr width height))
    (writefileraw (flat (cons header data)) name)) 
  (prn "Done.") ())

(def test-pic-create (name width height)
  " Init a BMP.  This is just test code I used to verify I was generating
    BMP headers correctly, as well as the body."
     (with (header (build-bmp-hdr width height) x 0 y 0 body () num_pad_bytes (mod (* width 3) 4))
       (for y 1 height
	    (for x 1 width
		 (= body (list body 0 0 255))) ; bgr byte order
	    (repeat num_pad_bytes (= body (list body 0))))
       (writefileraw (flat (cons header body)) name)))

;************************************************************
;* Util functions (general enough to pull out of this file) *
;************************************************************

(def clamp (value limit)
  " Clamp a given value so that it does not exceed limit."
  (if (> value limit)
      limit
      value))

(def gen-word (n)
  " Returns a little-endian ordered 2 byte word."
  (flat (list (bit-and n 255) 
	      (bit-and (bit-shift n -8) 255))))

(def gen-long (n)
  " Returns a little-endian ordered 4 byte world."
  (flat (list (bit-and n 255)
	      (bit-and (bit-shift n -8) 255)
	      (bit-and (bit-shift n -16) 255)
	      (bit-and (bit-shift n -24) 255))))

(def hex_digit (n)
  " Given a number from 0 to 15, return a string containing a hex digit."
     (if (and (< n 16) (>= n 0))
	 (case n
	   10 "A"
	   11 "B"
	   12 "C"
	   13 "D"
	   14 "E"
	   15 "F"
	   (num n))))

(def to_hex (n)
  " Convert a number from decimal to a hex string." 
     (apply string (flat (to_h n))))

(def to_h (n)
  " Helper function for to_hex.
    See also [[to_hex]]"
       (if (~is n 0)
           (cons (to_h (quotient n 16)) (hex_digit (mod n 16)))))

