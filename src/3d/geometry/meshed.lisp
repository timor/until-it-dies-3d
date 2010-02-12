;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)

(defproto =meshed= (=3dobject= =compilable=) 
  (faces))

(defmessage faces (meshed)
  (:documentation "every =meshed= must provide a face list here, with correctly set up topology"))

;;returns a list of all edges contained in the =meshed=, in unspecified order
(defreply edges ((m =meshed=))
	  (loop with unique-edges = ()
	     for f in (faces m) do
	       (loop for e in (edges f)
		  do (pushnew e unique-edges))
	       finally (return unique-edges)))

(defreply vertices ((m =meshed=))
  (loop with unique-vertices = ()
     for f in (faces m) do
     (loop for v in (vertices f)
	do (pushnew v unique-vertices))
     finally (return unique-vertices)))

;;DONE: change the face-normals into vertex normals once done
(defreply draw ((m =meshed=) &key)
  (loop 
     for f in (faces m) do 
     (gl:with-primitive :polygon
       (loop
	  for v in (vertices f) 
	  for tc in (tex-coords f)
	  for ns in (vertex-normals-in f) do
	  (apply #'gl:normal (coerce ns 'list))
	  (when tc
	    (apply #'gl:tex-coord (coerce tc 'list)))
	  (apply #'gl:vertex (coerce (point v) 'list)))
       )))


;;DONE: fix index loop here
;;TODO: rewrite into auto-smooth
(defreply auto-normals ((m =meshed=))
	  (with-properties (vertices faces face-normals vertex-normals edgeloop-normals) m
	    (loop for ivec across faces
	       collect (apply #'3p-normal (map 'list (lambda (i)
						       (svref vertices i))
					       ivec)) into normals
	       finally (setf face-normals (coerce normals 'simple-vector)))
	    ;;loop over all vertex indices, get all the faces that contain the vertex and average their normals
	    (loop for vi from 0 below (length vertices)
	       for ilist = (positions vi faces :test 'find)
	       collect (loop for ni in ilist
			  collect (svref face-normals ni) into nlist
			  finally (return (normalize! (apply #'vector+ nlist)))) into vnorms
	       finally (setf vertex-normals (coerce vnorms 'simple-vector)))))

(defreply apply-transformation ((trans =transformation=) (meshed =meshed=))
  (loop for v in (vertices meshed) do
       (setf (point v)
	     (apply-transformation trans (point v)))))


;;;;;;;;;=========texture coordinate generation==================
;;cylindrical tex-coord generation:
;; check the quadrants of face vertices:
;; if any two vertices are in the first and fourth quadrant of the cylindrical gizmo, overlapping tex-coords must be created

(defun project-point-along-normal (point plane)
  (let ((n (normal plane)))
    (vector+ point
	     (dot-product (dot-product (vector- 0 point)
				       n)
			  n))))
;;TODO: insert new transformation code here

;;DOING: rewrite with new transformation code
(defun generate-cylindrical-tex-coords (meshed)
  (declare (optimize (debug 3) (speed 0) (compilation-speed 0)))
  (let* ((vertices (vertices meshed))
	 (points (mapcar 'point vertices))
	 ;;	 (center (apply '3p-average points))
	 (z-extent (loop for p in points
		      maximize (3dp-z p) into zmax
		      minimize (3dp-z p) into zmin
		      finally (return (list zmin zmax))))
	 (zmin (first z-extent))
	 (zmax (second z-extent))
	 (zdiff (- zmax zmin)))
    (loop for vertex in vertices
       for point = (point vertex)
       for x = (3dp-x point)
       for y = (3dp-y point)
       for z = (3dp-z point)
       for last-phi_i = 0 then phi_i
       for phi_i = (+ pi (atan y x))
       for dphi = (if (< phi_i last-phi_i)
		      (progn
			(print "break_over")
			(- (+ (* 2 pi) phi_i)
			   last-phi_i))
		      (- phi_i last-phi_i))
       for phi = 0 then (+ phi dphi)
       for u = (/ phi
		  (* 2 pi))
       for v = (clamp (/ (- z zmin)
			 zdiff) 0 1)
       do
       (format t "x: ~a y: ~a phi_i: ~a last-phi_i: ~a u: ~a~% phi: ~a dphi: ~a~%" x y phi_i last-phi_i u phi dphi)
       (setf (tex-coords vertex) (vector (float u) (float v))))))

(defun point-quadrant (point-in-cyl-coords)
  "determines a point's quadrant in current coordinates as cylindrical
coords around #(0 0 1); there is the special quadrant 0 for points having r= 0"
  (let ((phi (svref point-in-cyl-coords 1))
	(rho (svref point-in-cyl-coords 0)))
    (cond ((tol= rho 0)
	   0)
	  ((tol>= phi (* pi 3/2))
	   4)
	  ((tol>= phi pi)
	   3)
	  ((tol>= phi pi/2)
	   2)
	  (t 1))))


;;FIXME: with-accessors is probably stupid here, because faces needs to be a property anyways
(defun generate-cylindrical-tex-coords (meshed &key (origin #(0 0 0)) (up-vector #(0 0 1)))
  "set the tex-coords property of all faces to be able to render a
texture wrapped around meshed in cylindrical coordinates"
  (declare (optimize (debug 3) (speed 0) (compilation-speed 0)))
  (with-accessors ((faces faces)) meshed
    (let* ((all-points (mapcar #'point (vertices meshed)))
	   (transformation 	    (make =cylindrical-coordinate-transformation= 
					  :new-up-vector up-vector 
					  :new-origin (or origin (apply #'3p-average all-points))))
	   zmax 
	   zmin)
      (loop for p in all-points 
	 for z = (svref p 2)
	 maximize z into zmax-temp
	 minimize z into zmin-temp
	 finally (setf zmax zmax-temp
		       zmin zmin-temp))
      (loop for face in faces
	 for face-number from 0 by 1
	 for vertices = (vertices face)
	 for points = (mapcar (fun (apply-transformation transformation (point _))) vertices)
	 for point-quadrants = (mapcar #'point-quadrant points) do
	 (format t "face number: ~a~%" face-number)
	 (let ((invalid-face-p (or (set-equal '(1 2 3 4) point-quadrants) 
				   (member 0 point-quadrants) ;TODO this has to go away
				   (parallel-vectors-p (normal face) up-vector)))
	       (wrap-around-p
		(or 
		 ;;points surely crossing the phi=0 boundary
		 (set-equal '(4 1) point-quadrants) 
		 (set-equal '(1 4 3) point-quadrants)
		 ;;point maybe crossing the phi=0 boundary
		 (and (set-equal '(1 4) point-quadrants)
		      ;;check the normalized plane normal for
		      ;;direction in respect to a vector pointing at
		      ;;the quadrant diagonal
		      (> 0 (dot-product #(1 1 0)
					(normal (plane face))))
		      (format t "special case!!!!!~%")))))
	   (setf (tex-coords face)
		 (when (not invalid-face-p)
		   (loop for p in points
		      for q in point-quadrants
		      for rho = (svref p 0)
		      for z = (svref p 2)
		      for phi = (+ (svref p 1)
				   (if (and wrap-around-p
					    (= q 1))
				       2pi
				       0))
		      collect (vector (/ phi 2pi)
				      (/ (- zmax z)
					 (- zmax zmin)))))))))))