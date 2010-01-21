;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)

;;TODO: switch from hash-tables to arrays somehow
;;DOING: 2D-Noise
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
	  (error "trying to get noise value at nx=~a, which is bigger than nxsize=~a"nx nxsize)
	  (let* ((index (+ nx (* nxsize ny)))
		 (val (gethash index vals)))
	    (if val 
		val
		(setf (gethash index vals) (vector (random 1.0) (random 1.0)))))))))
;;TODO
(defun make-noise-fn-3d (nxsize nysize)
  )



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