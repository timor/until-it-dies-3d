;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

;;make-point with floats, needed for glu. not nice, has to go away
(deftype 3dp ()
  '(vector double-float 3))

(defun 3dp (x y &optional (z 0))
  (vector (float x) (float y) (float z)))

(defun 3dp-x (p)
  (aref p 0))
(defun 3dp-y (p)
  (aref p 1))
(defun 3dp-z (p)
  (aref p 2))

;;;first, get us a 3d view
(defproto =3dview= () 
  ((eye (3dp 10 20 15))
   (center (3dp 0 0 0))
   (up (3dp 0 0 1))
   (fov 45)
   (aspect 1)
   ;;following are just copied from 3dsheep, need to updated when resized
   width
   height))


;;pretty not-so-bright to write this, setf is much shorter, but unsafer too :(
;;actually, its superseded by move-to in =camera=, but we dont know that here...
(defmessage update-3dview (view &key))
(defreply update-3dview ((v =3dview=) &key (eye (eye v)) (center (center v)) (up (up v)))
	  (check-type eye 3dp)
	  (check-type center 3dp) 
	  (check-type up 3dp)
	  (setf (eye v) eye
		(center v) center
		(up v) up))

;;for now put default lighting in here
(defreply set-view ((view =3dview=))
	  (with-properties (width height fov aspect eye center up) view
	    (gl:clear :depth-buffer-bit)
	    (gl:viewport 0 0 width height)
	    (gl:matrix-mode :projection)
	    (gl:load-identity)
	    (glu:perspective fov aspect 0.1 1000)
	    (glu:look-at (3dp-x eye) (3dp-y eye) (3dp-z eye)
			 (3dp-x center) (3dp-y center) (3dp-z center)
			 (3dp-x up) (3dp-y up) (3dp-z up))
	    (gl:matrix-mode :modelview)
	    (gl:light :light0 :position #(-0.2 0.3 1 0))
	    (gl:enable :lighting :light0 :color-material)
	    (gl:load-identity)))


;;TODO: check delegation of vectors so they are overriden when changed...
;;want the object functionality here, as well as the move-to
(defproto =camera= (=3dview= =3dobject=)
  ((phi 1)
   (theta 1)
   (rho 1)))

;;some craziness, lets see if that works
(defun change-view-to-camera (view)
  (unless (descendantp view =camera=)
    (setf (object-parents view)
	  (list =camera=))))

;;special thingy to harmonize the two definitions
(defreply move-to :after ((cam =camera=) new-pos)
	  (declare (ignorable new-pos))
	  (setf (eye cam) (vector 
			   (float (x cam)) 
			   (float (y cam))
			   (float (z cam)))))

(defreply orbit-by ((cam =camera=) drho dphi dtheta)
	  (with-properties (eye center phi rho theta) cam
	    (let* ((cx (3dp-x center))
		   (cy (3dp-y center))
		   (cz (3dp-z center))
		   (x (- (3dp-x eye) cx))
		   (y (- (3dp-y eye) cy))
		   (z (- (3dp-z eye) cz)))
	      (setf rho (max 0.1 (sqrt (+ (expt x 2) (expt y 2) (expt z 2))))
		    theta (acos (/ z rho))
		    phi (atan y x))
	      (setf rho (max 0.1 (+ rho drho))
		    phi (+ phi (deg2rad dphi))
		    theta (clamp (+ theta (deg2rad dtheta)) (- pi) pi 0.01))
	      (move-to cam (3dp
			    (+ cx (* rho (sin theta) (cos phi)))
			    (+ cy (* rho (sin theta) (sin phi)))
			    (+ cz (* rho (cos theta))))))))