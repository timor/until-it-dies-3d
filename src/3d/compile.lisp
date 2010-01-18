;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;DOING: changing display list handling to something simpler, making the whole hooks and containers concept uneccessary

;;new display list handling:
;; * on the first drawing of the object, the display list is assigned and compiled
;; * when a change happend to the object, schedule-recompile must be called
;; * when discarding the object, the gc makes sure that the display list is freed, eventually
;;   however, the only way to force freeing them is by manually initiating a gc cyle, i fear

;;IMPORTANT: since draw wrapping for compilation is handled in an :around, care must be taken where to put specialized draw code. safest places are the :before and :after replies


;;DONE: make sure display list does _not_ contain transformation. -> added :no-transform to draw of 3dobject
;;REM DONE: hook into the add-content and remove-content messages

;;DOING: compilable objects
;; DONE: think where the available list indices should be stored and managed, probably in the engine... not needed, since genlists gives us a valid index and we dont have to track our own
(in-package #:uid)


;;working directly on =3dobject= should be possible, since everything that can be drawn, can be put into a display list, although specialized objects should implement their own compile function

;;first implementation will be with display lists
(defproto =compilable= ()
  ((need-recompile t)
   display-list-id
   ))

(defreply schedule-recompile ((c =compilable=))
  (setf (need-recompile c) t))

(defreply schedule-recompile ((l =list=))
  (dolist (o l)
    (schedule-recompile o)))

(defreply recompile-all ((e =3dsheep=))
  (dolist (c (3d-content e))
    (when (and (objectp c)
	       (ancestorp =compilable= c))
      (schedule-recompile c))))

(defreply (setf display-list-id) :around (new-val (c =compilable=))
	  (declare (ignore new-val))
	  (if (display-list-id c)
	      (error "dont your ever ever ever dare touch me")
	      (call-next-reply)))

;;must be called in opengl context
(defreply register-with-opengl ((c =compilable=))
  (let ((id (%gl:gen-lists 1)))
    (setf (display-list-id c)
	  id)
    (finalize c (lambda ()
		  (%gl:delete-lists id 1)))))

(defreply compile-display ((c =compilable=))
  (with-properties ((dli display-list-id)) c
    (when (null dli)
      (register-with-opengl c))
    (gl:with-new-list ((display-list-id c) :compile)
      (draw c :directly t :no-transform t))
    (setf (need-recompile c) nil)))

;;directly: dont check for recompile, would get recursive
(defreply draw :around ((o =compilable=) &key directly)
	  (if directly
	      (call-next-reply)
	      (progn
		(when (need-recompile o)
		  (compile-display o))
		(%gl:call-list (display-list-id o)))))

