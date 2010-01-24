;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)


;;TODO: think about making these things objects, to better debug their mem-requirements and stuff
;;TODO: switch from hash-tables to arrays somehow
;;DOING: ND-Noise with hash functions
;;noise generating closures and their henchmen

(defun interpolate-perlin (x)
  "perlins interpolation function, modified for better curvature"
  (* (expt x 3)
     (+ (* x
	   (- (* x 6)
	      15 ))
	10)))

;;funny noise hash:
(defun make-noise-fn-1d ()
  "returns a (lambda(n) ...) that gives a repeatable random value for each n"
  (let ((vals (make-hash-table :test 'eq)))
    (lambda (n)
      (let ((val (gethash n vals)))
	(if val
	    val
	    (setf (gethash n vals) (random 1.0)))))))

(defun make-noise-fn-2d (nxsize)
  "makes a closures that returns 2d vectors on each grid position (lambda(nx ny)...)
for efficiency reasons, the grid size in x must be given"
  (let ((vals (make-hash-table :test 'eq)))
    (lambda (nx ny)
      (assert (and (>= nx 0) (>= ny 0)))
      (if ( >= nx nxsize)
	  (error "trying to get noise value at nx=~a, which is bigger than nxsize=~a" nx nxsize)
	  (let* ((index (+ nx (* nxsize ny)))
		 (val (gethash index vals)))
	    (if val 
		val
		(setf (gethash index vals) (vector (random 1.0) (random 1.0)))))))))
;;DOING
(defun make-noise-fn-3d (nxsize nysize)
  (let ((vals (make-hash-table :test 'eq)))
    (lambda (nx ny nz)
      (assert (and (>= nx 0)
		   (>= ny 0)
		   (>= nz 0)))
      (if (or (>= nx nxsize)
	      (>= ny nysize))
	  (error "(~a,~a) out of bounds of (~a,~a)" nx ny nxsize nysize)
	  (let* ((index (+ (* nz nysize nxsize)
			   (* nxsize ny)
			   nx))
		 (val (gethash index vals)))
	    (if val 
		val
		(setf (gethash index vals) (vector (random 1.0) (random 1.0) (random 1.0)))))))))



;;;;;;;;===========actual perlin functions
(defun make-one-perlin-noise-1d ()
  (flet ((interpolate (xu w0 w1)
	   (let ((sx (interpolate-perlin xu)))
	     (+ (* (- 1 sx) w0)
		(* sx w1)))))
    (let ((nf (make-noise-fn-1d)))
      (lambda (x)
	(let* ((p (floor x))
	       (g0 (funcall nf p))
	       (g1 (funcall nf (1+ p)))
	       (xu (- x p))
	       (w0 (* xu g0))
	       (w1 (* (- xu 1) g1)))
	  (interpolate xu w0 w1))))))

(defun make-perlin-noise-1d (fmin fmax persistence)
  (let ((pf (make-one-perlin-noise-1d)))
    (lambda (x)
      (loop for f = fmin then (* 2 f)
	 for n from 0 by 1
	 for a = persistence then (* persistence a)
	 while (<= f fmax)
	 sum (* a (funcall pf (* f x)))))))


;;=====2d=========
(defun make-one-perlin-noise-2d (nxsize)
  (let* ((2dpf (make-noise-fn-2d nxsize)))
    (flet ((interpolate (xu yu w00 w10 w01 w11)
	     (let* ((sx (interpolate-perlin xu))
		    (sy (interpolate-perlin yu))
		    (w0 (+ (* (- 1 sx) w00)
			   (* sx w10)))
		    (w1 (+ (* (- 1 sx) w01)
			   (* sx w11))))
	       (+ (* (- 1 sy) w0)
		  (* sy w1)))))
      (lambda (x y)
	(let* ((px (floor x))
	       (py (floor y))
	       (g00 (funcall 2dpf px py))
	       (g10 (funcall 2dpf (1+ px) py))
	       (g01 (funcall 2dpf px (1+ py)))
	       (g11 (funcall 2dpf (1+ px) (1+ py)))
	       (xu (- x px))
	       (yu (- y py))
	       (w00 (dot-product g00 (vector xu yu)))
	       (w10 (dot-product g10 (vector (- xu 1) yu)))
	       (w01 (dot-product g01 (vector xu (- yu 1))))
	       (w11 (dot-product g11 (vector (- xu 1) (- yu 1)))))
	  (interpolate xu yu w00 w10 w01 w11))))))

(defun make-perlin-noise-2d (fmin fmax persistence xmax)
  (let* ((nxsize (ceiling (* fmax xmax)))
	 (2dpf (make-one-perlin-noise-2d nxsize)))
    (lambda (x y)
      (loop for f = fmin then (* 2 f)
	   for n from 0 by 1
	   for a = persistence then (* persistence a)
	   while (<= f fmax)
	   sum (* a (funcall 2dpf (* f x) (* f y)))))))

;;===========n-d===============
(defparameter *most-positive-32-bit-int* (1- (expt 2 31)))

(defun hash-int-ward (i)
  "one method to obtain a 32 bit integer hash on a given integer number"
  (let ((k (logxor i
		   (ash i 13))))
    (logand *most-positive-32-bit-int*
     (+ 1376312589
	(* k
	   (+ 789221
	      (* (expt k 2)
		 15731))
	 )))))

(defun hash-int-shift (i)
  (let (k)
    (setf k (+
	     (lognot i)
	     (ash i 15))
	  k (logxor k (ash k -12))
	  k (+ k
	       (ash k 2))
	  k (logxor k
		    (ash k -4))
	  k (* k 2057)
	  k (logxor k
		    (ash k -16)))
    (logand k *most-positive-32-bit-int*)))


;;TODO: test speed with map code in lambda
(defun make-n-dimensional-noise (&optional (seed (random *most-positive-32-bit-int*)))
  (lambda (vec)
    (coerce (loop 
	       for i from 0 below (length vec)
	       collect (/
			(funcall 'hash-int-shift (loop 
						    for j from 0 by 1
						    for d across vec
						    sum (+ seed i j (* d (+ seed j i)))))
			*most-positive-32-bit-int*
			1.0))
	    'vector)))

#|
(defun make-3-dimensional-noise (&optional (seed (random *most-positive-32-bit-int*)))
  (lambda (d1 d2 d3)
    (vector (hash-int-shift (+ 0 seed (* d1 (+ seed 1)) (* d2 (+ seed 2)) (* d3 (+ seed 3))))
	    (hash-int-shift (+ 1 seed (* d1 (+ seed 4)) (* d2 (+ seed 5)) (* d3 (+ seed 6))))
	    (hash-int-shift (+ 2 seed (* d1 (+ seed 7)) (* d2 (+ seed 8)) (* d3 (+ seed 9)))))))

(defmacro make-n-dimensional-noise (n &optional (seed (random *most-positive-32-bit-int*)))
  (let ((coords (loop for i from 1 to n collect (intern (format nil "D~d" i))))
	(divisor (* 1.0 *most-positive-32-bit-int*)))
    `(let ((seed ,seed))
       (lambda (,@coords)
	 (vector ,@(loop for j from 1 to n collect 
		      `(/ (hash-int-shift
			   (+ ,j seed
			      ,@(loop for d in coords
				   for i from 1 by 1 collect
				   `(* ,d (+ seed ,(+ (* n j) i))))))
			  ,divisor)))))))

|#

(defmacro dp (label &rest things)
  `(format *error-output*  "~a:~%~{~a: ~a~%~}end~%" ',label (list ,@(loop for thing in things append `(',thing ,thing)))))

(defun make-perlin-noise (&optional (seed (random *most-positive-32-bit-int*)))
  "make some noise!"
  (when (null seed) (setf seed (random *most-positive-32-bit-int*)))
  (lambda (x)
    (when (numberp x)
      (setf x (vector x)))
    (labels ((vertex (j n)
	       (loop for k from 0 below n
		  collect (logand 1 (ash j (- k)))
		  into v
		  finally (return (coerce v 'vector))))
	     (interpolate (xu w &optional (d 0))
	       ;;(dp interpolate-args xu w d)
	       (if (= 1 (length w))
		   (first w)
		   (progn
		     (let* ((xu_d (svref xu d))
			    (s (+ (* 10 (expt xu_d 3))
				  (- (* 15 (expt xu_d 4)))
				  (* 6 (expt xu_d 5))))
			    (m (/ (length w) 2))
			    (w* (loop for i from 0 below m 
				   for wa = (nth (* 2 i) w)
				   for wb = (nth (1+ (* 2 i)) w)
				     ;;do (dp interpolation s i m wa wb)
				   collect
				   (+ (* (- 1 s) wa)
				      (* s wb)))))
		       (interpolate xu w* (1+ d)))))))
      (let* ((grad (make-n-dimensional-noise seed))
	     (n (length x))
	     (k (expt 2 n))
	     (p0 (map 'vector 'floor x))
	     (Q (loop for j from 0 below k collect (vertex j n)))
	     (G (loop for qj in Q collect (funcall grad (vector+ p0 qj))))
	     (xu (vector- x p0))
	     (w (loop for qj in Q
		   for gj in G
		   collect (dot-product gj (vector- xu qj)))))
	;;(dp noise-calcs grad n k p0 Q G xu w)
	(interpolate xu w)))))