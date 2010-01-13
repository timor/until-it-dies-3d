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
	  (gl:with-pushed-attrib  (:lighting-bit)
	    (gl:disable :lighting)
	    (with-properties (p0 px py pz) axes
	      (draw-line p0 px :color *red*)
	      (draw-line p0 py :color *blue*)
	      (draw-line p0 pz :color *green*))))


;;(add-content te *axes*)
(defreply draw :before ((e te) &key)
	  (draw *axes*))

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
(setf (numsegs r1) 5)
(turn r1)
(add-content te r1)

(setf (numsegs r1) 20)

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

;;now create a camera
(defparameter tv (current-3dview te))

(change-view-to-camera tv)

(defreply update ((cam tv) dt &key)
	  (orbit-by cam 0 (* dt 10) 0))

(defreply update ((eng te) dt &key)
	  (update (current-3dview te) dt))

(defun v3 (vec)
  (create =vertex= :point vec))

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

;;just for testing now, can only do flat stuff
(defreply draw ((f =face=) &key)
	  (let ((n (normal f)))
	    (gl:with-primitive :polygon
	      (gl:normal (svref n 0) (svref n 1) (svref n 2))
	      (loop for v in (mapcar #'point (vertices f)) do
		   (gl:vertex (svref v 0) (svref v 1) (svref v 2))))))

;;create the first face

(defparameter f1 (create =face= :vertices (mapcar (fun (nth _ cvs)) (first cinds))))
(defparameter f3 (create =face= :vertices (mapcar (fun (nth _ cvs)) (car (last cinds)))))

(defparameter f2 (create =face= :vertices (mapcar (fun (nth _ cvs)) (second cinds))
			 :neighbors (list f1 f3)))

(loop for f in (list f1 f2 f3) do
     (add-content te f))