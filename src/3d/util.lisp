;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;TODO: the tolerance stuff should probably be put in a different file
;;TODO: try to make a new type for precision-limited numbers
;; about tolerances: whenever some point in space is created, 


(in-package #:uid)

(defparameter *tolerance* 1e-5)

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

(defun maptree(fn &rest args)
  "recursive mapcar for trees"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar 
	     #'(lambda (&rest args)
		 (apply #'maptree fn args))
	     args)))


(defun positions (item sequence &key from-end (start 0) end key test test-not)
  "like position, but return all positions of item in sequence"
  (let ((found (position item sequence :from-end from-end :start start :end end :key key :test test :test-not test-not)))
    (when found
      (cons found 
	    (positions item sequence :from-end from-end :start (+ 1 found) :end end :key key :test test :test-not test-not)))))



;;DONE: need to normalize normals :)
(defun 3p-normal (p1 p2 p3 &optional pin)
  "calculate the normal for the given points, where points are in ccw order, or alternatively pi is a point facing away from the normals"
  (declare (ignorable pin))
  (normalize!
   (cross-product-3d (vector-between p1 p2)
		     (vector-between p2 p3))
   )
  )


;;math
(defun cross-product-3d (u v)
  (let ((ux (svref u 0))
	(uy (svref u 1))
	(uz (svref u 2))
	(vx (svref v 0))
	(vy (svref v 1))
	(vz (svref v 2))
	)
    (vector (- (* uy vz) (* uz vy))
	    (- (* uz vx) (* ux vz))
	    (- (* ux vy) (* uy vx)))))

(defun vector-between (base tip)
  "compute vector between two points, pointing from base to tip"
  (coerce 
   (loop for a across base
      for b across tip
      collect (- b a))
   'simple-vector))


(defun vector+ (&rest vecs)
  "vector addition"
  (apply #'map 'vector #'+ vecs))

(defun normalize! (vec)
  "normalize a simple vector"
  (let ((length (sqrt (loop for a across vec sum
			   (expt a 2)))))
    (loop for i from 0 below (length vec)
       for e = (svref vec i)  do
	 (setf (svref vec i) (/ e length)))
    vec))

(defun 3p-average (&rest points)
  (map 'vector (fun (/ _ (length points))) (apply #'vector+ points)))

(defun deg2rad (x)
  (* pi (/ x 180.0)))

(defun clamp(val min max &optional margin)
  "limit val between min and max, optionally inset by margin, such that the extremes may not be met"
  (if margin
      (min (- max margin) (max (+ min margin) val))
      (min max (max min val))))

;;plane handling
(defproto =plane= ()
  ((a 0)
   (b 0)
   (c 1)
   (d 0)
   (ref #(1 0 0))))

(defreply tol= ((a =number=) (b =number=) &key (tolerance *tolerance*))
  (<= (abs (- a b))
      tolerance))

(defreply tol= ((a =vector=) (b =vector=) &key (tolerance *tolerance*))
	  (when (= (length a) (length b))
	    (reduce (lambda (x y) (and x y))
		    (map 'list (lambda (i j) (tol= i j :tolerance tolerance))
			 a b))))

(defun point-on-plane (point plane &key (tolerance *tolerance*))
  (with-properties (a b c d) plane
    (tol= (+ (* a (point-x point))
	  (* b (point-y point))
	  (* c (point-z point)))
       d :tolerance tolerance)))

;;;;ordered number set:
;;(defun find-gap-in-ons (ons)
;;  "returns the first number that is in a gap")

(defun printv (thing)
  (print-sheeple-object-verbose thing *standard-output*))

