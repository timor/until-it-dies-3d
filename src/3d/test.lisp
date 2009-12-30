;;some test code, gets loader after everything, by default


;;TODO: this has to be external!!!!
(in-package #:uid)

;;test objects
(defparameter te (create =3dsheep=))

#+sbcl 
(run-in-thread te)
#-sbcl 
(run te)

(defparameter *axes* (create =3dobject=
			     'p0 (make-point 0 0 0)
			     'px (make-point 1 0 0)
			     'py (make-point 0 1 0)
			     'pz (make-point 0 0 1))) 

(defreply draw ((axes *axes*) &key)
	  (with-properties (p0 px py pz) axes
	    (draw-line p0 px :color *red*)
	    (draw-line p0 py :color *blue*)
	    (draw-line p0 pz :color *green*)))


(pushnew *axes* (3d-content te))
(defparameter c1 (list
	    #(1 0)
	    #(2 1)
	    #(1.5 2)
	    #(2 3)
	    #(2 4)
	    #(1 5)))

(defparameter r1 (create =rotary=
		   'curve c1
		   'numsegs 5))
(turn r1)
(pushnew r1 (3d-content te))

(setf (numsegs r1) 20)

(turn r1)
(move-to r1 #(5 5 0))

;;now create a camera
(defparameter tv (current-3dview te))

(change-view-to-camera tv)

(defreply update ((cam tv) dt &key)
	  (orbit-by cam 0 (* dt 50) 0))

(defreply update ((eng te) dt &key)
	  (update (current-3dview te) dt))

