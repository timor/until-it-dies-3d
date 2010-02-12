;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:until-it-dies)

;;;simple matrix calculations, not fast, not safe, but fun

(deftype matrix ()
  '(array * (* *)))

(defun equidimensional-array-p (a)
  (or (< (array-rank a) 2))
  (apply #'= (array-dimensions a)))

(deftype square-matrix ()
  `(and matrix
	(satisfies equidimensional-array-p)))

(defun identity-matrix (dim)
  (let ((res (make-array (list dim dim) :initial-element 0)))
    (loop for i from 0 below dim do
	 (setf (aref res i i) 1))
    res))

(defun map-matrix-elements (fun &rest matrices)
  "maps fun on all arguments, all matrices must be of equal or higher
extents than the first matrix, returns a new matrix with the result"
  (loop for m in matrices do
       (assert (typep m 'matrix)))
  (let* ((m1 (first matrices))
	 (new-matrix (copy-array m1)))
    (loop for row from 0 below (array-dimension m1 0) do
	 (loop for column from 0 below (array-dimension m1 1) do
	      (setf (aref new-matrix row column)
		    (apply fun (loop for m in matrices collect (aref m row column))))))
    new-matrix))

(defun matrix-*-elements (mat num)
  (map-matrix-elements (curry #'* num) mat))

(defun matrix+ (&rest matrices)
  (apply 'map-matrix-elements #'+ matrices))

(defun matrix- (&rest matrices)
  (apply 'map-matrix-elements #'- matrices))

(defun matrix-row (mat row-number &optional (return-type 'vector))
  (loop for column from 0 below (array-dimension mat 1)
     collect (aref mat row-number column) into res
     finally (return (coerce res return-type))))

(defun matrix-column (mat col-number &optional (return-type 'vector))
  (loop for row from 0 below (array-dimension mat 0)
     collect (aref mat row col-number) into res
     finally (return (coerce res return-type))))

;;FIXME: these aren't really ensuring...yet
(defun ensure-matrix (maybe-vector)
  (if (= (array-rank maybe-vector) 1)
      (make-array (list 1 (array-dimension maybe-vector 0)) :initial-contents (list maybe-vector))
      maybe-vector))

(defun ensure-vector (maybe-matrix)
  (if (typep maybe-matrix '(array * (1 *)))
      (matrix-row maybe-matrix 0)
      maybe-matrix)) 

(defun transpose-matrix (mat)
  (let ((mat (ensure-matrix mat)))
    (let* ((m (array-dimension mat 0))
	   (n (array-dimension mat 1))
	   (res (make-array (list n m))))
      (loop for row from 0 below n do
	   (loop for col from 0 below m do
		(setf (aref res row col) (aref mat col row))))
      res)))

(defun tilde-matrix (vec)
  (assert (typep vec '(array number (3))))
  (let ((rx (svref vec 0))
	(ry (svref vec 1))
	(rz (svref vec 2)))
   (make-array '(3 3) :initial-contents
	       (list
		(list 0 (- rz) ry)
		(list rz 0 (- rx))
		(list (- ry) rx 0)))))

(defun matrix* (m1 m2)
  (let ((m1 (ensure-matrix m1))
	(m2 (ensure-matrix m2)))
    (assert (typep m1 'matrix))
    (assert (typep m2 'matrix)) 
    (assert (= (second (array-dimensions m1)) (first (array-dimensions m2))))
    (let ((result (make-array (list  (array-dimension m1 0) (array-dimension m2 1)))))
      (loop 
	 for row from 0 below (array-dimension m1 0) do
	 (loop for column from 0 below (array-dimension m2 1) do
	      (setf (aref result row column)
		    (dot-product (matrix-row m1 row) (matrix-column m2 column)))))
      result)))