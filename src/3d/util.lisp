;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

(defparameter *tolerance* 1e-5)

(defun maptree(fn &rest args)
  "recursive mapcar for trees"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar 
	     #'(lambda (&rest args)
		 (apply #'maptree fn args))
	     args)))

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


(defun normalize! (vec)
  "normalize a simple vector"
  (let ((length (sqrt (loop for a across vec sum
			   (expt a 2)))))
    (loop for i from 0 below (length vec)
       for e = (svref vec i)  do
	 (setf (svref vec i) (/ e length)))
    vec))

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

(defun tol= (a b &key (tolerance *tolerance*))
  (<= (abs (- a b))
      tolerance))

(defun point-on-plane (point plane &key (tolerance *tolerance*))
  (with-properties (a b c d) plane
    (tol= (+ (* a (point-x point))
	  (* b (point-y point))
	  (* c (point-z point)))
       d :tolerance tolerance)))

