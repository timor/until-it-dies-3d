;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)

;;simple checkerboard textured plane

(defparameter test-tex (make =checker-board-texture= 'spacing 0.125 'width 256 'height 256))

;;FIXME: this is not a =plane=!!!

(defproto =checkerboard= (=plane= =textured=)
  ((texture test-tex)))

(defproto engine (=editor=))

(run-in-thread engine)
(defparameter test-object (make =checkerboard=))

;;(add-content engine (make =checkerboard=))
(clear engine)

(defproto to2 (=rotary= =textured=)
  ((texture test-tex)
   (curve (make =curve=
		       #(1 0)
		       #(2 1)
		       #(1.5 2)
		       #(2 3)
		       #(2 4)
		       #(1 5)))))

(turn to2)
(generate-cylindrical-tex-coords to2)

(add-content engine to2)
