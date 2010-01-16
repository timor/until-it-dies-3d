;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

;;simple curve that knows its normal and which corner vertices are smooth
;;curve points are pairs of the form (position smoothp) or just simple points
(defproto =curve= ()
  (curve-points))

(defun ensure-curve-point (thingy)
  (if (listp thingy)
      (when (and (typep (first thingy) '(vector number 2))
		 (or (eq (second thingy) nil)
		     (eq (second thingy) t)))
	thingy)
      (if (typep thingy '(vector number 2))
	  (list thingy nil)
	  (error "cant make curve point out of: ~a" thingy))))

(defreply points ((c =curve=))
  "returns only the position part of a point"
  (mapcar 'first (curve-points c)))

(defreply smoothp-list ((c =curve=))
  "return a list with corresponding smoothp values of curve points, useful for iterating"
  (mapcar 'second (curve-points c)))

(defreply make ((proto =curve=) &rest curve-points)
  (call-next-reply proto 'curve-points
		   (mapcar 'ensure-curve-point curve-points)))

(defreply point-normals ((c =curve=))
  (append (list nil)
	  (loop for sublist on (points c)
	     for p1 = (first sublist)
	     for p2 = (second sublist)
	     for p3 = (third sublist)
	     while p3
	     collect (let ((v1 (vector+ (vector-between p1 p2)
					(vector-between p2 p3))))
		       (normalize! (vector (svref v1 1)
					   (- (svref v1 0))))))
	  (list nil)))
