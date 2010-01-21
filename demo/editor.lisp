;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;;goal: have  demo application(in this case an editor) to make use of the 3d extension
;;state: starting line...
;;TODO: make 2d stuff work again

(defpackage #:uid-demo-3d
  (:use #:cl #:uid #:sheeple)
  (:export :run-demo))

(in-package #:uid-demo-3d)


(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defproto =editor= =3dsheep=
  ((title "simple 3d editing demo")
   (last-mouse-x 0)
   (last-mouse-y 0)
   (text-messages '())))

(defparameter *random-object*
  (let ((xmin 2.0)
	(xmax 4.0))
    (make =rotary= 
	  'numsegs 10
	  'curve (apply 'make =curve= (loop for z from 0 to 5 collect
					   (vector (+ xmin (random (- xmax xmin)))
						   z))))))

(defun run-demo ()
  (turn *random-object*)
  (when (3d-content =editor=) 
    (clear =editor=))
  (add-content =editor= *random-object*)
  #+sbcl (run-in-thread =editor=)
  #-sbcl (run =editor=))

(defparameter *camera*
  (make =camera=))

(setf (current-3dview =editor=) *camera*)
(setf (center *camera*) #(0d0 0d0 3d0))

(defparameter *axes* (make =3dobject=
			     'p0 (make-point 0 0 0)
			     'px (make-point 1 0 0)
			     'py (make-point 0 1 0)
			     'pz (make-point 0 0 1))) 


;;2d-content is probably bullshit
;;3d-content too, yippey, because of how the display list handling used to be
(defreply draw-3d :before ((e =editor=) &key)
  (gl:with-pushed-attrib  (:lighting-bit)
    (gl:disable :lighting)
    (with-properties (p0 px py pz) *axes*
      (draw-line p0 px :color *red*)
      (draw-line p0 py :color *blue*)
      (draw-line p0 pz :color *green*))))

;;;now for the mouse moving stuff
(defvar *cam-move-mode* nil)

(defreply mouse-move :after ((e =editor=) x y)
	  (with-properties ((lastx last-mouse-x) (lasty last-mouse-y)) e
	    (setf lastx x
		  lasty y)))

(defreply mouse-move ((e =editor=) x y)
  (with-properties (last-mouse-x last-mouse-y) e
    (case *cam-move-mode*
      (:orbit
       (orbit-by *camera* 0 (- last-mouse-x x) (- y last-mouse-y))))))

(defreply mouse-down ((e =editor=) button)
  (case button 
    (2 (setf *cam-move-mode* :orbit))
    (3 (orbit-by *camera* -5 0 0))
    (4 (orbit-by *camera* 5 0 0))))

(defreply mouse-up ((e =editor=) button)
  (case button 
    (2 (setf *cam-move-mode* nil))))


;;font:
(defproto *our-font* uid:=font=
  ((uid:filepath (merge-pathnames "example.otf" *resource-directory*))))

(defreply draw-2d ((e =editor=) &key)
  (with-font *our-font*
    (let ((msgs (text-messages =editor=)))
	 (when msgs
	   (loop for i from 10 by 20
	      for msg in msgs do
		(draw-at 10 i (format nil msg)))))))

(push "hello world" (text-messages =editor=))