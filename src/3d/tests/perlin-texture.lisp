
(in-package #:until-it-dies)

(defparameter engine (make =editor=))

(defparameter tex (make =perlin-noise-texture= 'width 128 'height 128))

(defproto plane (=3dobject= =textured=)
  ((texture tex)))

(defreply draw ((p plane) &key)
  (gl:with-primitive :quad
    (gl:vertex 0 0)
    (gl:tex-coord 0 0)
    (gl:vertex 1 0)
    (gl:tex-coord 1 0)
    (gl:vertex 1 1)
    (gl:tex-coord 1 1)
    (gl:vertex 0 1)
    (gl:tex-coord 0 1)))