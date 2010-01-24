;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;;this editor is nifty to have around, just make it your parent and youll have a mouse controlled camera

(in-package #:until-it-dies)

(defproto =editor= =3dsheep=
  ((title "editor mode")
   (last-mouse-x 0)
   (last-mouse-y 0)
   (cam-move-mode nil)))

(defreply shared-init :after ((editor =editor=) &key)
	  (change-view-to-camera (current-3dview editor)))

(defreply draw-3d :before ((editor =editor=) &key)
	  (let ((p0 (make-point 0 0 0))
		(px (make-point 1 0 0))
		(py (make-point 0 1 0))
		(pz (make-point 0 0 1)))
	    (gl:with-pushed-attrib (:lighting-bit)
	      (gl:disable :lighting)
	      (draw-line p0 px :color *red*)
	      (draw-line p0 py :color *blue*)
	      (draw-line p0 pz :color *green*))))

(defreply mouse-move :after ((e =editor=) x y)
	  (with-properties ((lastx last-mouse-x) (lasty last-mouse-y)) e
	    (setf lastx x
		  lasty y)))

(defreply mouse-move ((e =editor=) x y)
  (with-properties (last-mouse-x last-mouse-y cam-move-mode (camera current-3dview)) e
    (case cam-move-mode
      (:orbit
       (orbit-by camera 0 (- last-mouse-x x) (- y last-mouse-y))))))

(defreply mouse-down ((e =editor=) button)
  (with-properties ((camera current-3dview) cam-move-mode) e
    (with-properties (rho) camera
     (case button 
       (0 (setf cam-move-mode :orbit))
       (3 (orbit-by camera (- (* rho 0.1)) 0 0))
       (4 (orbit-by camera (* rho 0.1) 0 0))))))

(defreply mouse-up ((e =editor=) button)
  (with-properties (cam-move-mode) e
    (case button 
      (0 (setf cam-move-mode nil)))))
