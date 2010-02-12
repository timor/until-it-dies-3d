;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;;this editor is nifty to have around, just make it your parent and youll have a mouse controlled camera
;;TODO: integrate the provided-res stuff into the asdf file

(in-package #:until-it-dies)

(defvar *provided-resource-directory*
  (merge-pathnames "provided-res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defproto =editor= =3dsheep=
  ((title "editor mode")
   (last-mouse-x 0)
   (last-mouse-y 0)
   (hud-font (make =font= :filepath (merge-pathnames "example.otf" *provided-resource-directory*)))
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
	      (draw-line p0 py :color *green*)
	      (draw-line p0 pz :color *blue*))))

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


;;dunno if we always want this but hey, it would be nice if it worked in teh first place :/
(defreply draw-2d ((editor =editor=) &key)
  (with-properties ((font hud-font) window-height window-width) editor
    (with-color *yellow*
      (draw-line (make-point 0 0) (make-point 100 100))
      (with-font font
	(draw-at 10 (- window-height 20) (format nil "Mean FPS: ~,3f" (float (mean-fps editor) 1.0)))))))
