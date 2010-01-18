;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

;;ACTUALLY: check if below stuff can be accomplished by normal event handling
;;DONE: containers: may contain other objects, and decide to modify their replies
;;       more importantly, every object registers its hooks with its container,
;;TOOD:  which in turn must register them in his, asf.


;;DOING: purging hooks
;;DOING: purging =container= maybe add later for other purpose

;;containers are for storing other objects, they must keep track of hooks, which may be stored there
;;hooks are one-time closures, anything else is an update, not a hook
;;hooks are provided to execute gl commands in the correct context

;;This is called before draw, so that update hooks are processed which need to be done before displaying something

;;kind of like a uid :)
(defproto =3dsheep= (=engine=)
  (current-3dview
   2d-content
   3d-content))

;;DONE: test on PC where gl stuff works -> does
;;this is needed to ensure proper execution context, use this if collisions with draw routines exist
(defmacro run-in-context (engine &body code)
  `(fork (:queue (event-queue ,engine))
     ,@code))

(defreply (setf current-3dview) :after (new-val (e =3dsheep=))
	  (declare (ignore new-val))
	  (with-properties (width height aspect) (current-3dview e)
	    (setf aspect (/ (window-width e) (window-height e))
		  width (window-width e)
		  height (window-height e))))

;;TODO: check if that redundancy is needed, shouldnt be
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
;;TODO: ensure that 2d stuff still works
;;DOING: split into draw-2d and draw-3d?

;;default replies
(defreply draw-2d ((e =3dsheep=) &key)
  (draw (2d-content e)))

(defreply draw-3d ((e =3dsheep=) &key)
  (draw (3d-content e)))

(defmacro with-pushed-gl-context (&body code)
  `(gl:with-pushed-attrib (:all-attrib-bits)
     (%gl:matrix-mode :projection)
     (gl:with-pushed-matrix 
       (%gl:matrix-mode :modelview)
       (gl:with-pushed-matrix 
	 ,@code))))

(defreply draw ((e =3dsheep=) &key)
  (with-pushed-gl-context
    (set-view (current-3dview e))  
    (draw-3d e))
  (with-pushed-gl-context
    (set-view (current-view e))
    (draw-2d e)))

;;draw all kinds of things
(defreply draw ((l =list=) &key)
  (dolist (li l)
    (draw li)))

(defreply add-content ((e =3dsheep=) o &key (view :3d))
  "this is the preferred way of adding things to an engine, view can be :2d or :3d"
  (run-in-context e
    (ecase view
      (:2d (pushnew o (2d-content e)))
      (:3d (pushnew o (3d-content e))))))

(defreply remove-content ((e =3dsheep=) (o =3dobject=) &key)
  "remove content from 2d and 3d view of engine"
  (run-in-context e
    (setf (2d-content e) (remove o (2d-content e)))
    (setf (3d-content e) (remove o (3d-content e)))))

(defreply clear ((e =3dsheep=))
  "remove all 3d content from engine"
  (if (runningp e)
      (run-in-context e
	(loop for c in (3d-content e) do
	     (remove-content e c)))
      (loop for c in (3d-content e) do
	   (remove-content e c))))

;now lets get our repl back!========================
	  
#+sbcl
(defun run-in-thread (engine)
  (if (find '3dsheep-thread
	    (maptree #'sb-thread:thread-name (sb-thread:list-all-threads))
	    :test #'eql)
      ;;already have engine running
      :already-have-threaded-engine
      (sb-thread:make-thread (lambda () (run engine)) :name '3dsheep-thread)))

