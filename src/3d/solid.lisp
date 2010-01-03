;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;simple solid body creation, lotsa stuff can be made outa that
;;TODO: finish rotary
;; DONE: basic lathing functionality
;; TODO: need to implement round shading by switching to vertex normals

;;DONE: think about putting faces into separate data-type, letting meshed take care of extracting information
;; DOING: put the new datatype in here

(in-package #:uid)

(defproto =meshed= (=3dobject=)
  (vertices ;;make-point thingies!!!
   faces
   face-normals
   vertex-normals))

;;DOING: change the face-normals into vertex normals once done
(defreply draw ((m =meshed=) &key)
	  (with-properties (vertices faces (normals vertex-normals)) m
	    (loop for flist across faces 
	       ;;for normal across normals
	       do 
	       (gl:with-primitive :polygon
		 (loop for f across flist do
		      (let ((v (svref vertices f))
			    (n (svref normals f)))
			(gl:normal (svref n 0) (svref n 1) (svref n 2))
			(gl:vertex (svref v 0) (svref v 1) (svref v 2))))
		 ))))


;;DONE: need to normalize normals :)
(defun 3p-normal (p1 p2 p3 &optional pin)
  "calculate the normal for the given points, where points are in ccw order, or alternatively pi is a point facing away from the normals"
  (declare (ignorable pin))
  (normalize!
   (cross-product-3d (vector-between p1 p2)
		     (vector-between p2 p3))
   )
  )

;;UTIL:
(defun positions (item sequence &key from-end (start 0) end key test test-not)
  (let ((found (position item sequence :from-end from-end :start start :end end :key key :test test :test-not test-not)))
    (when found
      (cons found 
	    (positions item sequence :from-end from-end :start (+ 1 found) :end end :key key :test test :test-not test-not)))))

;;DOING: normals should be fixed for smooth shading, test old normals with flat shading, though
;; DOING: normals look really bad, debug that
;;DONE: fix index loop here
(defreply auto-normals ((m =meshed=))
	  (with-properties (vertices faces face-normals vertex-normals edgeloop-normals) m
	    (loop for ivec across faces
	       collect (apply #'3p-normal (map 'list (lambda (i)
						       (svref vertices i))
					       ivec)) into normals
	       finally (setf face-normals (coerce normals 'simple-vector)))
	    ;;loop over all vertex indices, get all the faces that contain the vertex and average their normals
	    (loop for vi from 0 below (length vertices)
	       for ilist = (positions vi faces :test 'find)
	       collect (loop for ni in ilist
			  collect (svref face-normals ni) into nlist
			  finally (return (normalize! (apply #'vector+ nlist)))) into vnorms
	       finally (setf vertex-normals (coerce vnorms 'simple-vector)))))


;;bring out the lathe===============================================
(defproto =rotary= (=meshed=)
  ((curve) ;;2d vertices in x-z plane
   (numsegs 5)))

;;DONE: change face orientation by flipping vertex order 
;;DONE: check orientation of faces practically, harmonize with normals
;;DONE: find out how 2d clipping affects 3d projection -> depth test was messing up
(defreply turn ((r =rotary=))
	  (with-properties (numsegs curve) r
	    (assert (>= numsegs 3))
	    (loop 
	       with cpoints = (length curve)
	       with numverts = (* cpoints numsegs)
	       with zenith =  numverts
	       with nadir = (1+  zenith)
	       for vnum from 0 by cpoints
	       for angle from 0 below (* 2 pi) by (/ (* 2 pi) numsegs)
	       ;;collect vertices
	       append (loop 
			 for ov in curve
			 for ovx = (svref ov 0)
			 for ovz = (svref ov 1)
			 collect (vector (* (cos angle) ovx) (* (sin angle) ovx) ovz)
			 ) into vertices
	       ;;collect faces
	       append (loop
			   with reverse = t
			 for i from 0 to cpoints
			 for j from (1-  vnum) by 1
			 collect (let ((res 
					(cond ((= i 0)
					       (vector zenith
						       (1+  j)
						       (mod (+ 1 j cpoints) numverts)
						       ))
					      ((= i cpoints)
					       (vector nadir
						       (mod (+ j cpoints) numverts)
						       j
						       ))
					      (t
					       (vector j
						       (+ j 1)
						       (mod (+ j cpoints 1) numverts)
						       (mod (+ j cpoints) numverts)
						       )))))
				   (if reverse
				       (reverse res)
				       res))) into faces
	       finally 
	       ;;need to add zenith and nadir here
	       (progn
		 (setf (vertices r) (concatenate 'simple-vector vertices
						 (list
						  ;;zenith
						  (vector 0 0 (svref (first curve) 1))
						  ;;nadir
						  (vector 0 0 (svref (car (last curve)) 1))))
		       (faces r) (coerce faces 'simple-vector))
		 (auto-normals r)
		 ))))