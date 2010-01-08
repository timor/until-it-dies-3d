(in-package #:uid)


;;nice lil' shortcut for interactive stuff placement
(defmessage move-to (object new-position)
  (:documentation  "move object to new position"))

(defmessage compile-display (object)
  (:documentation "compile an object so that it can be drawn faster"))

;;used and implemented in compile.lisp
(defmessage request-display-list (engine)
  (:documentation "get a display list from an engine"))

(defmessage add-content (container thingy &key))

(defmessage remove-content (container thingy &key))