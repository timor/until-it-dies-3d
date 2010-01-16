(in-package #:uid)

;;=============3d=======
(defmessage draw-2d (engine &key)
  (:documentation "the 'top-level' drawing routine for the engine in 2d context"))

(defmessage draw-3d (engine &key)
  (:documentation "see abovish"))

;;========3dobject=============
;;nice lil' shortcut for interactive stuff placement
(defmessage move-to (object new-position)
  (:documentation  "move object to new position"))


(defmessage add-content (container content-thingy &key))

(defmessage remove-content (container content-thingy &key))

;;===========util/debug========
(defreply print-sheeple-object-verbose (object stream))

;;=======compilable============
(defmessage compile-display (compilable-object)
  (:documentation "compile an object so that it can be drawn faster"))

(defmessage schedule-recompile (compilable-object)
  (:documentation "make sure compilable-object is recompiled before next draw cycle"))

;;========rotary================
(defmessage turn (rotary))


;;========topology=============
(defmessage used-by (usee user))

(defmessage normal (plany-thingy))

(defmessage next-in-face (face-boundary-thingy))

(defmessage attach (what where))

(defmessage neigbors (topological-thingy))