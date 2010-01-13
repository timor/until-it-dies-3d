;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;display list handling:
;; * on first adding to a container, a display list id is generated and assigned
;; * this id is used for recompilation and drawing
;; * when the object is added via add-content, its locks will contain the container object
;; * when the container releases the object, and no more locks are present, the display list is freed
;; * when that happens, the display list is freed
;; * and because all that must happen inside a gl context the list adding and deleting functions will be hooks in the main loop of the engine, which means that these must be some kind of gl-containers

;;IMPORTANT: since draw wrapping for compilation is handled in an :around, care must be taken where to put specialized draw code. safest places are the :before and :after replies


;;DONE: make sure display list does _not_ contain transformation. -> added :no-transform to draw of 3dobject
;;DONE: hook into the add-content and remove-content messages

;;DOING: compilable objects
;; DONE: think where the available list indices should be stored and managed, probably in the engine... not needed, since genlists gives us a valid index and we dont have to track our own
(in-package #:uid)


;;working directly on =3dobject= should be possible, since everything that can be drawn, can be put into a display list, although specialized objects should implement their own compile function

;;first implementation will be with display lists
(defproto =compilable= ()
  ((need-recompile t)
   display-list-id
   locks ;;this gets updated when added as content to something
   ))

(defreply schedule-recompile ((c =compilable=))
	  (setf (need-recompile c) t))

(defreply recompile-all ((e =3dsheep=))
	  (dolist (c (3d-content e))
	    (when (ancestorp =compilable= c)
	      (schedule-recompile c))))

(defreply compile-display ((c =compilable=))
	  (if (null (display-list-id c))
	      (error "trying to compile object without valid display list, did you just push something into a content list without using add-content?")
	      (progn
		(gl:with-new-list ((display-list-id c) :compile)
		  (draw c :directly t :no-transform t))
		(setf (need-recompile c) nil))))

;;directly: dont check for recompile, would get recursive
(defreply draw :around ((o =compilable=) &key directly)
	  (if directly
	      (call-next-reply)
	      (progn
		(when (need-recompile o)
		  (compile-display o))
		(%gl:call-list (display-list-id o)))))

;;when adding to a container the first time, allocate a display list
(defreply add-content :before ((container =container=) (object =compilable=) &key)
	  (when (null (locks object))
	    (if (display-list-id object)
		(error "have a display list id, but no locks, someone was a bad boy!")
		(push (lambda ()
			(setf (display-list-id object) (%gl:gen-lists 1)))
		      (hooks container))))
	  (pushnew container (locks object)))

;;when removing from container, maybe release the display list
(defreply remove-content :after ((container =container=) (object =compilable=) &key)
	  (with-properties (locks) object
	    (setf locks (remove container locks))
	    (if (null (locks object))
		(let ((id (display-list-id object)))
		  (when id
		    (push (lambda ()
			    (%gl:delete-lists id 1))
			  (hooks container))
		    (setf (display-list-id object) nil))))))