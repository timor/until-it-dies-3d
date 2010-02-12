
(in-package #:until-it-dies)

(defproto engine (=editor=)
  ((cam-speed 10)))

(defreply update ((eng engine) dt &key)
  (with-properties (cam-speed (camera current-3dview)) eng
    (orbit-by camera 0 (* dt cam-speed) 0)))

(defparameter tex (make =perlin-noise-texture= 'width 256 'height 256 'fmin 5))

(defproto =my-plane= (=rectangle= =textured=)
  ((texture tex)))

(run-in-thread engine)
(add-content engine (make =my-plane=))
(clear engine)

;;FIXME:see if this gets any better when run from inside
(defun reload-planes ()
  (clear engine)
  (loop
     with width = 32
     for i from 0 below 64
     for fmin from 0.5 by 2
     for j = (truncate (/ i 8))
     for tex = (make =perlin-noise-texture= 'width width 'height width 'fmin fmin)
     for plane = (make =my-plane= 'texture tex 'x (mod i 8) 'y j)
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