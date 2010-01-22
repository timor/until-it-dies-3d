;;some test code
;;it safest to execute every command in order by hand
;;when the engine thread signals an error, it is usually safe to continue,
;;any display errors should be fixed with the last command down there 


;;TODO: this has to be external!!!!
(in-package #:uid)

;;test objects
(defproto te =3dsheep=
  ((last-mouse-x 0)
   (last-mouse-y 0))
  )

#+sbcl
(run-in-thread te)
#-sbcl 
(run te)

(defparameter *axes* (make =3dobject=
			     'p0 (make-point 0 0 0)
			     'px (make-point 1 0 0)
			     'py (make-point 0 1 0)
			     'pz (make-point 0 0 1))) 

(defreply draw ((axes *axes*) &key)
	  (gl:with-pushed-attrib  (:lighting-bit)
	    (gl:disable :lighting)
	    (with-properties (p0 px py pz) axes
	      (draw-line p0 px :color *red*)
	      (draw-line p0 py :color *blue*)
	      (draw-line p0 pz :color *green*))))


;;(add-content te *axes*)
(defreply draw-3d :before ((e te) &key)
	  (draw *axes*))

;;dont see anything because the 3d-content of te is empty

(defparameter c1 (make =curve=
		       #(1 0)
		       #(2 1)
		       #(1.5 2)
		       #(2 3)
		       #(2 4)
		       #(1 5)))

(defparameter c2 (apply 'make =curve=
		       (mapcar (fun (list (car _) t))
			       (curve-points c1))))

(defparameter r1 (make =rotary=
		   'curve c1
		   'numsegs 5))
(setf (numsegs r1) 5)
(turn r1)
(add-content te r1)

(setf (numsegs r1) 12)

(turn r1)
(schedule-recompile r1)
(move-to r1 #(5 5 0))
;;show me the normals:
(defreply draw :after ((r r1) &key)
	  (gl:with-pushed-attrib (:lighting-bit)
	    (gl:disable :lighting)
	    (loop for f in (faces r) do
		 (draw-line (center f) (vector+ (center f) (normal f)) :color *magenta*)
		 (loop for v in (vertices f)
		    for n in (vertex-normals-in f r) do	    
		    (let* ((p1 (point v))
			   (p2 (vector+ p1 n)))
		      (draw-line p1 p2 :color *orange*))))))
(schedule-recompile r1)

;;now make a camera
(defparameter tv (current-3dview te))

(change-view-to-camera tv)

(defreply update ((cam tv) dt &key)
	  (orbit-by cam 0 (* dt 10) 0))

(defreply update ((eng te) dt &key)
	  (update (current-3dview te) dt))


;;actually drop that whole rotation thingy and make something useful:
(undefreply update (te =t=) )
(undefreply update (tv =t=))

(defvar *cam-move-mode* nil)
(defparameter *camera* (current-3dview te))

(defreply mouse-move :after ((e te) x y)
	  (with-properties ((lastx last-mouse-x) (lasty last-mouse-y)) e
	    (setf lastx x
		  lasty y)))

(defreply mouse-move ((e te) x y)
  (with-properties (last-mouse-x last-mouse-y) e
    (case *cam-move-mode*
      (:orbit
       (orbit-by *camera* 0 (- last-mouse-x x) (- y last-mouse-y))))))

(defreply mouse-down ((e te) button)
  (let ((dist (rho *camera*)))
    (case button 
      (0 (setf *cam-move-mode* :orbit))
      (3 (orbit-by *camera* (- (* dist 0.1)) 0 0))
      (4 (orbit-by *camera* (* dist 0.1) 0 0)))))

(defreply mouse-up ((e te) button)
  (case button 
    (0 (setf *cam-move-mode* nil))))

;;;===end of mouse cam===
(defun v3 (vec)
  (make =vertex= :point vec))

;;test the construction of a cube by hand
(defparameter cvs
    (list (v3 #(0 0 0))
	  (v3 #(0 1 0))
	  (v3 #(1 1 0))
	  (v3 #(1 0 0))
	  (v3 #(0 0 1))
	  (v3 #(0 1 1))
	  (v3 #(1 1 1))
	  (v3 #(1 0 1))))

(defparameter cinds
    '((0 1 2 3)
      (0 4 7 3)
      (0 1 5 4)
      (3 7 6 2)
      (2 6 5 1)
      (4 7 6 5)))


;;try to make sense of these vertex normals
(loop for f in (faces r1) do (setf (property-value f 'debug) nil)
   finally (schedule-recompile r1))

(defreply draw :after ((m =meshed=) &key)
	  (gl:with-pushed-attrib (:lighting-bit)
	    (gl:disable :lighting)
	    (loop for f in (faces m) do
		 (when (property-value f 'debug)
		   (draw-polygon (mapcar 'point (vertices f)) :filledp nil :color *blue*)))))

(defun debug-face (n &optional (on/off t))
  (setf (property-value (nth n (faces r1)) 'debug) on/off)
  (schedule-recompile r1))

(setf (property-value =face= 'color :accessor t) nil)

;;just for testing now, can only do flat stuff
(defreply draw ((f =face=) &key)
	  (let ((n (normal f)))
	    (gl:with-primitive :polygon
	      (when (color f) (bind-color (color f)))
	      (gl:normal (svref n 0) (svref n 1) (svref n 2))
	      (loop for v in (mapcar #'point (vertices f)) do
		   (gl:vertex (svref v 0) (svref v 1) (svref v 2))))))

;;make the first face

(defparameter f1 (make =face= :vertices (mapcar (fun (nth _ cvs)) (first cinds))))
(defparameter f3 (make =face= :vertices (mapcar (fun (nth _ cvs)) (car (last cinds)))))

(defparameter f2 (make =face= :vertices (mapcar (fun (nth _ cvs)) (second cinds))
			 :neighbors (list f1 f3)))

(loop for f in (list f1 f2 f3) do
     (add-content te f))


;;TODO: debug why display rotation hangs when recompiling :(

;;2d perlin noise test
(defproto *fun-display* (=3dobject= =compilable=)
  ((xmin 0)
   (xmax 20)
   (ymin 0)
   (ymax 20)
   func
   (fmin 0.5) ;;this determines the period of the largest component
   (tau 0.1) ;;display stepping distance, at min: pixel size on display
   (octaves 20) ;;shouldnt be needed if the sampling rate is known
   (persistence 0.5)
   (amplitude 10)))

(defreply draw ((fd *fun-display*) &key)
  (with-properties (fmin xmin xmax ymin ymax tau func persistence amplitude octaves) fd
    (let* ((fmax (* 0.5 (/ 1 tau))))
      (unless func
	(setf func (make-perlin-noise-2d fmin
					 ;;(* fmin (expt 2 octaves))
					 fmax
					 persistence
					 (- xmax xmin))))
      (loop for x from xmin to xmax by tau do
	   (loop for y from ymin to ymax by tau do
		(draw-point (make-point x y (* amplitude (funcall func x y))) :color *yellow*))))))

(defreply recalc ((fd *fun-display*))
  (setf (func fd) nil)
  (schedule-recompile fd))

(defreply (setf fmin) :after (new (fd *fun-display*))
	  (declare (ignore new))
	  (recalc fd))

(defreply (setf tau) :after (new (fd *fun-display*))
	  (declare (ignore new))
	  (recalc fd))

(defreply (setf persistence) :after (new (fd *fun-display*))
	  (declare (ignore new))	  
	  (recalc fd))


(add-content te *fun-display*)

;;and in the (very likely) case that some (contiuable) error messed up the display, this should fix things:
(recompile-all te)