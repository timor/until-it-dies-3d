;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;DOING: containers

(in-package #:uid)

;;containers are for storing other objects, they must keep track of hooks, which may be stored there
;;hooks are one-time closures, anything else is an update, not a hook
;;hooks are provided to execute gl commands in the correct context

(defproto =container= ()
  (hooks
   ))

;;This is called before draw, so that update hooks are processed which need to be done before displaying something
(defreply update :after ((c =container=) dt &key)
	  (declare (ignorable dt))
	  (with-properties (hooks) c
	    (when hooks 
	      (dolist (h hooks)
		(funcall h))
	      (setf (hooks c) '()))))

;;kind of like a uid :)
(defproto =3dsheep= (=engine= =container=)
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

;;DOING: cache the view matrices directly or as display lists
;;modify draw stuff to incorporate new 3dness
(defreply draw ((e =3dsheep=) &key)
	  (with-properties (2d-content 3d-content current-view current-3dview) e
	    (when 3d-content
	      (set-view current-3dview)
	      (draw 3d-content))
	    (when 2d-content
	      (set-view current-view)
	      (draw 2d-content))))

;;draw all kinds of things
(defreply draw ((l =list=) &key)
	  (dolist (li l)
	    (draw li)))

(defreply remove-content ((e =3dsheep=) (o =3dobject=) &key)
	  "remove content from 2d and 3d view of engine"
	  (setf (2d-content e) (remove o (2d-content e)))
	  (setf (3d-content e) (remove o (3d-content e))))

;;;basic 3d objects===================

;;generic 3d objects that have a position in space
(defproto =3dobject= ()
  ((x 0)
   (y 0)
   (z 0)))

(defreply add-content ((e =3dsheep=) (o =3dobject=) &key (view :3d))
	  "this is the preferred way of adding things to an engine, view can be :2d or :3d"
	  (ecase view
	    (:2d (pushnew o (2d-content e)))
	    (:3d (pushnew o (3d-content e)))))

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
;;actually: none of this is probably needed since smooth shading is enabled by default, and flat shading is just a special case
(defproto =shaded= ()
  ((smooth t)))

;;DONE: debug why this doesnt get called right... => need correct vertex normals
;;DONE: recalculate vertex normals correctly
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
