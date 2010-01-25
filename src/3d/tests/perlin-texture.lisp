
(in-package #:until-it-dies)

(defproto engine (=editor=)
  ((cam-speed 10)))

(defreply update ((eng engine) dt &key)
  (with-properties (cam-speed (camera current-3dview)) eng
    (orbit-by camera 0 (* dt cam-speed) 0)))

(defparameter tex (make =perlin-noise-texture= 'width 256 'height 256 'fmin 5))

(defproto =plane= (=3dobject= =textured=)
  ((texture tex)))

(defreply draw ((p =plane=) &key)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex 0 0)
    (gl:tex-coord 1 0)
    (gl:vertex 1 0)
    (gl:tex-coord 1 1)
    (gl:vertex 1 1)
    (gl:tex-coord 0 1)
    (gl:vertex 0 1)
    ))

(run-in-thread engine)
(add-content engine =plane=)
(clear engine)

(defun reload-planes ()
  (clear engine)
  (loop
     with width = 16
     for i from 0 below 64
     for fmin from 0.5 by 2
     for j = (truncate (/ i 8))
     for tex = (make =perlin-noise-texture= 'width width 'height width 'fmin fmin)
     for plane = (make =plane= 'texture tex 'x (mod i 8) 'y j)
     do
     (format t "plane number ~a has fmin:~a~%" i fmin)
     (add-content engine plane)))

(defun font-test-fn ()
  (lambda ()  (with-font (hud-font engine)
		(draw-at 2 2 "testestest"))))

(defun point-test-fn ()
  (lambda ()
    (with-color *green*
      (uid:draw-points (loop for i below 100 collect (make-point (random 10)
								 (random 10)
								 1))))))

(reload-planes)