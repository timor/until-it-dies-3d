;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

;;;basic 3d objects===================

;;generic 3d objects that have a position in space
(defproto =3dobject= ()
  ((x 0)
   (y 0)
   (z 0)))

(defreply move-to ((o =3dobject=) pos)
  (with-properties (x y z) o
    (setf x (elt pos 0)
	  y (elt pos 1)
	  z (elt pos 2))))

;;do the cool transformation stuff here
(defreply draw :around ((o =3dobject=) &key no-transform)
	  (with-properties (x y z) o
	    (gl:with-pushed-matrix 
	      (unless no-transform
		(gl:translate x y z))
	      (call-next-reply))))

;;shaded objects: this should get merged into normal objects, since it is really basic, but its a good exercise for mixins right now
;;actually: none of this is probably needed since smooth shading is enabled by default, and flat shading is just a special case
(defproto =shaded= ()
  ((smooth t)))

;;DONE: debug why this doesnt get called right... => need correct vertex normals
;;DONE: recalculate vertex normals correctly
(defreply draw :around ((o =shaded=) &key )
	  (gl:with-pushed-attrib (:lighting-bit)
	    (%gl:shade-model (if (smooth o)
				 :smooth
				 :flat))
	    (call-next-reply)))


