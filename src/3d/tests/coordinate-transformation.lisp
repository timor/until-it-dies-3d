;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)

(defproto editor =editor=)

(run-in-thread editor)

(defproto =arrow= =3dobject=
  (base 
   tip
   (color *red*)))

(defun vector-arrow (tip &key (base #(0 0 0)) (color *red*))
  (make =arrow= 'base base 'tip tip 'color color))

(defreply draw ((arrow =arrow=) &key)
  (with-properties (base tip color) arrow
   (gl:with-pushed-attrib (:lighting-bit)
     (gl:disable :lighting)
     (gl:with-primitive :lines
       (with-color *white*
	 (gl:vertex (svref base 0) (svref base 1) (svref base 2)))
       (with-color color
	 (gl:vertex (svref tip 0) (svref tip 1) (svref tip 2)))))))



(clear editor)

(defparameter new-up #(1 1 1))
(defparameter tr (make =cylindrical-coordinate-transformation=
		       :new-origin #(0 0 0)
		       :new-up-vector new-up
		       :reference-vector #(0 1 0)
		       ))

(progn
 (add-content editor (vector-arrow (apply-transformation tr #(1 0 0)) :color *red*))
 (add-content editor (vector-arrow (apply-transformation tr #(0 1 0)) :color *green*))
 (add-content editor (vector-arrow (apply-transformation tr #(0 0 1)) :color *blue*))
 (add-content editor (vector-arrow (apply-transformation tr new-up) :color *orange*)))

(clear editor)

(defparameter box (make =cuboid= 'width 1 'len 1 'height 1 'bypass t))

(add-content editor box)

(apply-transformation tr box)