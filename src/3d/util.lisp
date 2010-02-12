;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;TODO: the tolerance stuff should probably be put in a different file
;;TODO: try to make a new type for precision-limited numbers
;; about tolerances: whenever some point in space is compared, tol= should be used


(in-package #:uid)

(defparameter *tolerance* 1e-5)
(defparameter 2pi (* 2 pi))
(defparameter pi/2 (/ pi 2))
(defparameter pi/4 (/ pi 4))

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
		     (vector-between p2 p3))))


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

;;TODO: maybe make &rest vectors instead of u v
(defreply dot-product ((u =vector=) (v =vector=))
  (apply #'+ (map 'list #'* u v)))

(defreply dot-product ((u =number=) (v =number=))
  (* u v))

(defreply dot-product ((u =number=) (v =vector=))
  (map 'vector (fun
		 (* u _)) v))

(defreply dot-product ((u =vector=) (v =number=))
  (dot-product v u))


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

(defreply vector- ((v1 =vector=) (v2 =vector=))
  (map 'vector #'- v1 v2))

(defreply vector- ((v1 =vector=) (a =number=))
  (map 'vector (fun (- _ a)) v1))

(defreply vector- ((a =number=) (v1 =vector=))
  (map 'vector (fun (- a _)) v1))

(defun vector-abs (vec)
  (sqrt (loop for c across vec sum (expt c 2))))

(defun angle-between-vectors (vec1 vec2)
  (acos (/ (dot-product vec1 vec2)
	   (* (vector-abs vec1)
	      (vector-abs vec2)))))

(defun parallel-vectors-p (v1 v2)
  (let ((angle (angle-between-vectors v1 v2)))
    (or (tol= pi angle)
	(tol= 0 angle))))

;;DOING: having this destructive is outright stupid
(defun normalize! (vec)
  "normalize a simple vector"
  (let ((length (sqrt (loop for a across vec sum
			   (expt a 2)))))
    (loop for i from 0 below (length vec)
       for e = (svref vec i)  do
	 (setf (svref vec i) (/ e length)))
    vec))

(defun normalize (vec)
  (let ((length (sqrt (loop for a across vec sum (expt a 2)))))
    (apply 'vector (loop for vi across vec collect (/ vi length)))))

;;FIXME: maybe rename this to vector-average
(defun 3p-average (&rest points)
  (map 'vector (fun (/ _ (length points))) (apply #'vector+ points)))

(defun deg2rad (x)
  (* pi (/ x 180.0)))

;;DOING fixing margin on both ends
(defun clamp(val min max &optional margin)
  "limit val between min and max, optionally inset by margin, such that the extremes may not be met"
  (if margin
      (min (- max margin) (max (+ min margin) val))
      (min max (max min val))))

;;TODO: move to different file
;;plane handling
(defproto =plane= ()
  ((a 0)
   (b 0)
   (c 1)
   (d 0)
   (ref #(1 0 0))))

;;TODO
(defun make-plane-from-vectors (vec1 vec2)
  "returns a =plane= object that is spanned by vec1 and vec2")


;;TODO: make sure that d is always positive
(defun make-plane-from-normal (n &optional (dist-spec 0))
  "define a plane by giving a normal vector and the distance to the
origin, dist-spec may either be a number, or a point that the plane
shall contain, if the normal vector would result in a negative distance, it is flipped(IMPORTANT)"
  (let* ((nn (normalize n))
	 (d (if (numberp dist-spec)
		dist-spec
		(dot-product nn dist-spec))))
    (when (< d 0)
      (setf d (- d)
	    nn (vector- 0 nn)))
    (make =plane=
	  'a (svref nn 0)
	  'b (svref nn 1)
	  'c (svref nn 2)
	  'd d)))

(defreply normal ((plane =plane=))
  "return the normal of the plane. if it was flipped on plane creation, it is flipped here as well"
  (with-properties (a b c) plane
    (vector a b c)))

(defreply tol= ((a =number=) (b =number=) &key (tolerance *tolerance*))
  (<= (abs (- a b))
      tolerance))

(defreply tol= ((a =vector=) (b =vector=) &key (tolerance *tolerance*))
  (when (= (length a) (length b))
    (reduce (lambda (x y) (and x y))
	    (map 'list (lambda (i j) (tol= i j :tolerance tolerance))
		 a b))))

(defreply tol<= ((a =number=) (b =number=) &key (tolerance *tolerance*))
  (or (< a b)
      (tol= a b :tolerance tolerance)))

(defreply tol>= ((a =number=) (b =number=) &key (tolerance *tolerance*))
  (or (> a b)
      (tol= a b :tolerance tolerance)))

(defreply normal ((plane =plane=))
  "returns the normalized plane normal vetor"
  (with-properties (a b c) plane
      (normalize! (vector a b c))))

(defun point-on-plane (point plane &key (tolerance *tolerance*))
  (with-properties (a b c d) plane
    (tol= (+ (* a (point-x point))
	  (* b (point-y point))
	  (* c (point-z point)))
       d :tolerance tolerance)))

(defun printv (thing)
  (print-sheeple-object-verbose thing *standard-output*))

(defun mod* (x div)
  "actually returns the correct modulus"
  (if (< x 0)
      (- (mod x (- div)))
      (mod x div))) 

;;TODO maybe change arg names
(defun interpolate-linear (a b x)
  (+ (* a (- 1 x)) (* b x)))

(defun interpolate-cosine (a b x)
  (let ((f
	 (* 0.5 (- 1 (cos (* pi x)) ))))
    (+ (* a (- 1 f))
       (* b f))))

(defun interpolate-cubic (x g0 g1)
  "two-point cubic interpolation of gradient points, with x between 0 and 1"
  (+ (* g0 (expt (- x 1) 2) x)
     (* g1 (- x 1) (expt x 2))))
(defun interpolate-integer-1d (x fun &optional (i-fun 'interpolate-cosine))
  (let ((y1 (funcall fun (floor  x)))
	(y2 (funcall fun (ceiling x))))
    (funcall i-fun y1 y2 (mod x (if (< x 0) -1 1)))))

(defun random-permutate-sequence! (seq)
  (let ((l (length seq)))
    (loop for i from 0 below (1- l)
       for r = (+ i (random (- l i))) do
       ;;(format t "swapping ~a with ~a ~%" i r)
       (psetf (elt seq i) (elt seq r)
	      (elt seq r) (elt seq i)))
    seq))

