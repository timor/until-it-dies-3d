;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;display list handling:
;; * on first adding to a container, a display list id is generated and assigned
;; * this id is used for recompilation and drawing
;; * when the object is added via add-content, its locks will contain the container object
;; * when the container releases the object, and no more locks are present, the display list is freed
;; * when that happens, the display list is freed
;; * and because all that must happen inside a gl context the list adding and deleting functions will be hooks in the main loop of the engine, which means that these must be some kind of gl-containers


;;TODO: try display list overriding
;;TODO: hook into the add-content and remove-content messages

;;DOING: compilable objects
;; DOING: display-list struct with finalizer
;; DONE: think where the available list indices should be stored and managed, probably in the engine... not needed, since genlists gives us a valid index and we dont have to track our own
(in-package #:uid)


;;working directly on =3dobject= should be possible, since everything that can be drawn, can be put into a display list, although specialized objects should implement their own compile function

;;first implementation will be with display lists
(defproto =compilable= ()
  ((need-recompile t)
   display-list-id
   locks ;;this gets updated when added as content to something
   ))

(defreply display-list ((c =compilable=))
	  (gl-list-proxy-id (display-list-proxy c)))

(defreply compile-display ((c =compilable=))
	  (setf (valid c) nil)
	  (if (null (display-list c))
	      (error "trying to compile object without valid display list")
	      (gl:with-new-list (display-list c) :compile
		(draw c :directly t))))

;;directly: dont check for recompile, would get recursive
(defreply draw :around ((o =compilable=) &key directly)
	  (if directly
	      (call-next-reply)
	      (progn
		(when (need-recompile o)
		  (compile-display o))
		(call-next-reply))))

;;now this is something new: modify the engine object here with previuosly undefined properties, as well as hook ourself into the =3dobject= hierarchy
;;wether this is good style is a different question, but since nothing here will be used anywhere else, the result should be consistent
;;only drawback is that the definition is split in multiple places :<

;;another way to hook this in would be in the init function. implications unknown

;;TODO: put the right kind of eval-when here
;; i hate eval-when. its more like evil-when
;;NOTE: perhaps the member test should be replaced by an ancestor test
(let ((opars (copy-list (object-parents =3dobject=))))
  (unless (member =compilable= opars)
    (setf (object-parents =3dobject=) (pushnew =compilable= opars))))


;;when adding to a container the first time, allocate a display list
(defreply add-content :before ((container =container=) (object =compilable=))
	  (when (null (locks object))
	    (if (display-list c)
		(error "have a display list id, but no locks, someone was a bad boy!")
		(push (lambda ()
			(setf (display-list c) (%gl:gen-lists 1)))
		      (hooks container))))
	  (pushnew container (locks object)))

;;when removing from container, maybe release the display list
(defreply remove-content :after ((container =container=) (object =compilable=))
	  (with-properties (locks) object
	    (if (null (locks object))
		(let ((id (display-list-id object)))
		  (when id
		    (push (lambda ()
			    (%gl:delete-lists id 1))
			  (hooks container))
		    (setf (display-list-id object) nil)))
		(setf locks (remove container locks)))))