;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)


;;DONE: containers: may contain other objects, and decide to modify their replies
;;       more importantly, every object registers its hooks with its container,
;;TOOD:  which in turn must register them in his, asf.


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
	  (setf (current-3dview 3ds) (make =3dview=))
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

(defreply add-content ((e =3dsheep=) o &key (view :3d))
	  "this is the preferred way of adding things to an engine, view can be :2d or :3d"
	  (ecase view
	    (:2d (pushnew o (2d-content e)))
	    (:3d (pushnew o (3d-content e)))))

(defreply remove-content ((e =3dsheep=) (o =3dobject=) &key)
	  "remove content from 2d and 3d view of engine"
	  (setf (2d-content e) (remove o (2d-content e)))
	  (setf (3d-content e) (remove o (3d-content e))))

;now lets get our repl back!========================
	  
#+sbcl
(defun run-in-thread (engine)
  (if (find '3dsheep-thread
	    (maptree #'sb-thread:thread-name (sb-thread:list-all-threads))
	    :test #'eql)
      ;;already have engine running
      :already-have-threaded-engine
      (sb-thread:make-thread (lambda () (run engine)) :name '3dsheep-thread)))

;;TODO: test on PC where gl stuff works
;;this is needed to ensure proper execution context, use this if collisions with draw routines exist
(defmacro run-in-context (engine &body code)
  `(push (lambda ()
	   ,@code)
	 (hooks ,engine)))