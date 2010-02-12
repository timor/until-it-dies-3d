;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)

;;TODO: create cuboid on top of =3dobject=, mixin =meshed=, which requires the faces reply and try to modify the uv's

(defproto =cuboid= (=meshed=)
  ((width 1)  ;;corresponds to x
   (len 1)    ;; y
   (height 1) ;; z
   )) 

(defreply shared-init :after ((cuboid =cuboid=) &rest initargs)
	  (declare (ignorable initargs))
	  ;;create the topology here
	  (with-properties (width height len faces) cuboid
	    (setf faces
		  (let* ((coords (list #(0 0 0)  (vector 0 0 height) (vector 0 len 0) (vector 0 len height)
				       (vector width 0 0) (vector width 0 height) (vector width len 0) 
				       (vector width len height)))
			 (verts (mapcar (fun (make =vertex= :point _)) coords)))
		    (loop for inds in '((0 4 5 1) (4 6 7 5) (7 6 2 3) (1 3 2 0) (4 0 2 6) (1 5 7 3))
		       collect (make =face= :vertices (mapcar (fun (nth _ verts)) inds)))))))

#|
(defreply draw ((cuboid =cuboid=) &key)
  (with-properties (vertices) cuboid
    (gl:with-primitive :triangle-strip
      (loop for i in '(1 0 5 4 7 6 3 2 1 0)
	 do
	 (apply #'gl:vertex (coerce (point (nth i vertices)) 'list))))
    (gl:with-primitive :triangle-strip
      (loop for i in '(3 1 7 5)
	 do
	 (apply #'gl:vertex (coerce (point (nth i vertices)) 'list))))
    (gl:with-primitive :triangle-strip
      (loop for i in '(6 4 2 0)
	 do
	 (apply #'gl:vertex (coerce (point (nth i vertices)) 'list))))))
|#