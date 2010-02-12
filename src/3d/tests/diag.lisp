;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

;;want to print out opengl state

(defun print-gl-matrix (matrix-keyword)
  (let ((result (gl:get-double matrix-keyword)))
    (format t "~{~a ~a ~a ~a~%~}" (coerce result 'list))))

(defun print-gl-integer (integer-keyword)
  (let ((result (gl:get-integer integer-keyword)))
    (format t "gl integer ~a: ~a~%" integer-keyword result)))

(defmacro once-lambda (&body code)
  "creates a closure that executes code at most once"
  (with-gensyms (done)
    `(let ((,done nil))
       (lambda ()
	 (when (not ,done)
	   (prog1
	       (progn ,@code)
	     (setf ,done t))
	   )))))

(defparameter *eng* (make =editor=))

(defparameter *test-closure*
  (once-lambda
    (print-gl-matrix :projection-matrix)
    (print-gl-matrix :modelview-matrix)))

(add-content *eng*
	     (once-lambda
	       (format t "2d projection:~%")
	       (print-gl-matrix :projection-matrix)
	       (format t "2d modelview:~%")
	       (print-gl-matrix :modelview-matrix))
	     :view :2d)
(add-content *eng*
	     (once-lambda
	       (format t "3d projection:~%")
	       (print-gl-matrix :projection-matrix)
	       (format t "3d modelview:~%")
	       (print-gl-matrix :modelview-matrix))
	     :view :3d)
(add-content *eng*
	     (once-lambda
	       (loop for key in (list :max-projection-stack-depth :max-texture-stack-depth :max-modelview-stack-depth) do
		    (print-gl-integer key))))

(defparameter *2de* (make =engine=))

(defreply draw ((e *2de*) &key)
  (funcall *test-closure*))