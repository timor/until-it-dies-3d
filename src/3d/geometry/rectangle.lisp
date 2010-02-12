;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)


;;TODO: definitely need other name for that, rectangle would be better
(defproto =rectangle= =3dobject=
  ((width 1)
   (length 1 :accessor nil)))

(defreply draw ((rect =rectangle=) &key)
  "this one actually has valid tex-coords"
  (with-properties ((w width) (l length)) rect
   (gl:with-primitive :quads
     (gl:tex-coord 0 0)
     (gl:vertex 0 0)
     (gl:tex-coord 1 0)
     (gl:vertex w 0)
     (gl:tex-coord 1 1)
     (gl:vertex w l)
     (gl:tex-coord 0 1)
     (gl:vertex 0 l)
     )))