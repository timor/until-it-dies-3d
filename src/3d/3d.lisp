;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

;;kind of like a uid :)
(defproto =3dsheep= (=engine=)
  (current-3dview
   2d-content
   3d-content))

(defreply shared-init :after ((3ds =3dsheep=) &key)
	  (setf (current-3dview 3ds) (create =3dview=))
	  (with-properties (width height aspect) (current-3dview 3ds)
	    (setf aspect (/ (window-width 3ds) (window-height 3ds))
		  width (window-width 3ds)
		  height (window-height 3ds))))

(defreply init :after ((e =3dsheep=))
	  (gl:enable :depth-test))

;;TODO: cache the view matrices directly or as display lists
;;modify draw stuff to incorporate new 3dness
(defreply draw ((e =3dsheep=) &key)
	  (with-properties (2d-content 3d-content current-view current-3dview) e
	    (when 2d-content
	      (set-view current-view)
	      (draw 2d-content))
	    (when 3d-content
	      (set-view current-3dview)
	      (draw 3d-content))))

;;draw all kinds of things
(defreply draw ((l =list=) &key)
	  (dolist (li l)
	    (draw li)))

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
(defreply draw :around ((o =3dobject=) &key)
	  (with-properties (x y z) o
	    (gl:with-pushed-matrix 
	      (gl:translate  x  y  z)
	      (call-next-reply))))

;;shaded objects: this should get merged into normal objects, since it is really basic, but its a good exercise for mixins right now
;;actually: none of this is probably needed since smooth shading is enabled by default
(defproto =shaded= ()
  ((smooth t)))

;;DONE: debug why this doesnt get called right... => need correct vertex normals
;;TODO: recalculate vertex normals correctly
(defreply draw :around ((o =shaded=) &key)
	  (gl:with-pushed-attrib (:lighting-bit)
	    (%gl:shade-model (if (smooth o)
				 :smooth
				 :flat))
	    (call-next-reply)))


;now lets get our repl back!========================
	  
#+sbcl
(defun run-in-thread (engine)
  (if (find '3dsheep-thread
	    (maptree #'sb-thread:thread-name (sb-thread:list-all-threads))
	    :test #'eql)
      ;;already have engine running
      :already-have-threaded-engine
      (sb-thread:make-thread (lambda () (run engine)) :name '3dsheep-thread)))
