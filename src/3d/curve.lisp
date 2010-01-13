;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)


;;curve points are pairs of the form (position smoothp) or just simple points
(defproto =curve= ()
  (curve-points))

(defun curve-point-p (thingy)
  (when (listp thingy)
    (when (first thingy)
    (or (eq (second thingy) nil)
	(eq (second thingy) t)))))

(defreply create ((proto =curve=) &key curve-points)
  (call-next-reply 'curve-points
		   (loop for cp in curve-points collect
			(if (curve-point-p cp)
			    cp
			    (progn
			     (assert (typep cp 'vector))
			     (list cp nil))
			    ))))