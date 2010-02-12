;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)

;;TODO: decide wether to use transformation objects or not and harmonize all the definitions accordingly

;;FIXME: once better autoboxing is done, make all type specs =3dpoints=
(defreply translate-coordinates ((point =vector=) old-origin new-origin)
  (vector+ point (vector-between new-origin old-origin)))

;;version 1: matrix operations
;;(defun make-rotation-matrix (axis angle)
;;  (let* ((s (sin angle))
;;	 (c (cos angle))
;;	 (tt (- 1.0 c))
;;	 (~axis (tilde-matrix (normalize axis))))
;;    (matrix+ (identity-matrix 3)
;;	     (matrix-*-elements ~axis s)
;;	     (matrix-*-elements (matrix* ~axis ~axis) tt))))

;;version 2: direct filling of matrix

(defun make-rotation-matrix (axis angle)
  (let* ((s (sin angle))
	 (c (cos angle))
	 (tt (- 1.0 c))
	 (naxis (normalize axis))
	 (x (3dp-x naxis))
	 (y (3dp-y naxis))
	 (z (3dp-z naxis)))
    (format t "s: ~a c: ~a t: ~a~%" s c tt)
    (make-array (list 3 3) :initial-contents
		(list
		 (list (+ c (* tt x x))        (- (* tt x y) (* z s))  (+ (* tt x z) (* y s)))
		 (list (+ (* tt x y) (* z s))  (+ c (* tt y y))        (- (* tt y z) (* x s)))
		 (list (- (* tt x z) (* y s))  (+ (* tt y z) (* x s))  (+ c (* tt z z)))))))

(defun apply-rotation-matrix (rotmat point)
  (ensure-vector (transpose-matrix (matrix* rotmat (transpose-matrix point)))))

;;perhaps obsolete
(defreply rotate-coordinates ((points =list=) axis angle)
  (let ((rotmat (make-rotation-matrix axis (- angle))))
    (loop for point in points collect
	 (ensure-vector (transpose-matrix (matrix* rotmat (transpose-matrix point)))))))
 
(defreply rotate-coordinates ((point =vector=) axis angle)
  (first (rotate-coordinates (list point) axis angle)))

;;DOING: cylindrical coords,
;;MaybeTODO affine transformations
(defproto =transformation=)

;;this would be called apply, but that is already taken :(
(defmessage apply-transformation (transformation thing)
  (:documentation "transform a thing's coordinates")
  (:reply ((transformation =transformation=) (list =list=))
	  (dolist (i list)
	    (apply-transformation transformation i))))

(defproto =cylindrical-coordinate-transformation= =transformation=
    (;;new-origin
     ;;new-up-vector
     ;;reference-vector ;;if given, this will be projected
     ;;main-rotation-matrix ;; initialized on creation ;;obsolete
     rotation-matrix ;;used to rotate the reference in
			       ;;place, initialized on creation if
			       ;;reference-vector is given
     translation-vector ;;initialized on creation, responsible for moving origin
     ))


;;DOING: making sure the transformation is fully built here
(defreply make ((proto =cylindrical-coordinate-transformation=) &key (new-origin #(0 0 0)) new-up-vector reference-vector)
  "reference-vector is the projected axis that will have phi=0. if
reference-vector is omitted, the transformation will only take one rotation step"
  (if (and reference-vector
	   (parallel-vectors-p new-up-vector reference-vector))
      (error "projection of reference-vector would be singular on new base plane: ~a" reference-vector)
      (progn
	(assert (typep new-origin 'vector))
	(assert (typep new-up-vector 'vector))
	(let ((rotation-matrix
	       (let ((rotation-axis (cross-product-3d #(0 0 1) new-up-vector))
		     (rotation-angle (- (angle-between-vectors #(0 0 1) new-up-vector))))
		 (format t "rotation axis: ~a, rotation angle: ~a~%" rotation-axis rotation-angle)
		 (if (tol= rotation-axis #(0 0 0))
		     (identity-matrix 3)
		     (make-rotation-matrix rotation-axis rotation-angle)))))
	  (when reference-vector
	    (let* ((projected-reference (project-point-along-normal (apply-rotation-matrix rotation-matrix reference-vector)
								    (make-plane-from-normal #(0 0 1))))
		   (ref-angle (- (angle-between-vectors #(1 0 0) projected-reference))))
	      (setf rotation-matrix (matrix* (if (tol= 0 ref-angle)
						 (identity-matrix 3)
						 (make-rotation-matrix #(0 0 1) ref-angle))
					     rotation-matrix))))
	  (call-next-reply proto 
			   'rotation-matrix rotation-matrix
			   'translation-vector (vector-between new-origin #(0 0 0)))))))

(defreply apply-transformation ((trans =cylindrical-coordinate-transformation=)
				(vector =vector=))
  (with-properties (rotation-matrix translation-vector) trans
    (let* ((translated-vector (vector+ vector translation-vector))
	   (transformed-vector (apply-rotation-matrix rotation-matrix translated-vector)))
      (let* ((z (3dp-z transformed-vector))
	     (x (3dp-x transformed-vector))
	     (y (3dp-y transformed-vector))
	     (phi (max 0 (mod (if (= 0 y x)
				  0
				  (+ pi (atan y x)))
			      2pi)))
	     (rho (sqrt (+ (expt x 2) (expt y 2)))))
	(vector rho phi z)
	;;transformed-vector
	))))