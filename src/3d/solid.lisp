;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;simple solid body creation, lotsa stuff can be made outa that
;;TODO: finish rotary
;; DONE: basic lathing functionality
;; DONE: need to implement round shading by switching to vertex normals
;; TODO: have a way of describing in the curve which curve points are smooth and which are flat
;; TODO: define =curve= and (points =curve=) so we can have bezier curves and all that stuff, too

;;DOING: compilable objects

;;TODO: surface objects


(in-package #:uid)

(defproto =meshed= (=3dobject= =compilable=) 
  (faces))

;;returns a list of all edges contained in the =meshed=, in unspecified order
(defreply edges ((m =meshed=))
	  (loop with unique-edges = ()
	     for f in (faces m) do
	       (loop for e in (edges f)
		  do (pushnew e unique-edges))
	       finally (return unique-edges)))

;;DONE: change the face-normals into vertex normals once done
(defreply draw ((m =meshed=) &key)
	  (with-properties (faces) m
	    (loop 
	       for f in faces do 
	       (gl:with-primitive :polygon
		 (loop
		    for v in (vertices f) 
		    for ns in (vertex-normals-in f m) do
		    (apply #'gl:normal (coerce ns 'list))
		    (apply #'gl:vertex (coerce (point v) 'list)))
		 ))))

;;TODO: WTF!!!!!!!!!!!!!!!!!!!!! (some random bug that killed my whole image)
;;DOING: making that a reply on face and return a list of all the normals
;;TODO: use half-edgeity to only check concerning faces
(defreply vertex-normals-in ((face =face=) (m =meshed=))
	  (let (normal-faces ;this one is always included in the calculation
		(vertices (vertices face))) 
	    ;;look at all faces that contain the vertex
	    (loop for vertex in vertices 
	       do (setf normal-faces (list face))
	       collect
		 (normalize! (apply #'vector+ (mapcar #'normal (neighbors-at-vertex face vertex)))))))

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

;;DONE: fix index loop here
;;TODO: rewrite into auto-smooth
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
;;DONE: use new data structures, create a face and then attach another one
;; DONE: find out why neighboring does not work right >:(
;;TODO: curves should already contain the information wether their corners are smooth, this should be put into vertex-normals(=predefined-vertex-normals= mixin, perhaps) and used in drawing for speed gains
(defreply turn ((r =rotary=))
	  (with-properties (numsegs curve) r
	    (assert (>= numsegs 4)) ;;TODO find the bug with 3 segs
	    (let* (vertices
		   faces
		   (clength (length curve))
		   (numverts (* clength numsegs))
		   (zp (first curve))
		   (np (car (last curve)))
		   (zenith (create =vertex= :point (vector 0 0 (svref zp 1))))
		   (nadir (create =vertex= :point (vector 0 0 (svref np 1))))
		   )
	      (loop 
		 for vnum from 0 by clength
		 for angle from 0 below (* 2 pi) by (/ (* 2 pi) numsegs)
		 ;;collect vertices
		 append (loop 
			   for ov in curve
			   for ovx = (svref ov 0)
			   for ovz = (svref ov 1)
			   for i from 0 by 1
			   collect
			   (let ((new-vertex
				  (create =vertex=
					  :point (vector (* (cos angle) ovx)
							 (* (sin angle) ovx)
							 ovz))))
			     (setf (property-value new-vertex 'rotary-angle) angle
				   (property-value new-vertex 'rotary-original-point) ov
				   (property-value new-vertex 'rotary-point-number) (+ vnum i))
			     new-vertex)) into verts
		 finally (setf vertices verts))
	      ;;second run: build up the faces
	      (setf vertices (append vertices (list zenith nadir)))
	      (loop
		 with fs = ()
		 for vnum from 0 by clength
		 for angle from 0 below (* 2 pi) by (/ (* 2 pi) numsegs)
		 ;;collect faces (topology stuff)
		 do (loop
		       for i from 0 to clength
		       for j from (1-  vnum) by 1
		       do
		       (let ((new-face
			      (cond ((= i 0)
				     (create =face= :vertices
					     (list
					      (nth (1+  j) vertices)
					      zenith
					      (nth (mod (+ 1 j clength) numverts) vertices))
					     :neighbors fs
					     ))
				    ((= i clength)
				     (create =face= :vertices 
					     (list
					      (nth (mod (+ j clength) numverts) vertices)
					      nadir
					      (nth j vertices))
					     :neighbors fs
					     ))
				    (t
				     (create =face= :vertices
					     (mapcar (fun (nth _ vertices))
						     (reverse (list
							       j
							       (+ j 1)
							       (mod (+ j clength 1) numverts)
							       (mod (+ j clength) numverts)
							       )))
					     :neighbors fs
					     )))))
			 (setf (property-value new-face 'rotary-curve-segment) clength)
			 (loop for e in (edges new-face) do (setf (smoothp e) t))
			 (push new-face fs)))
		 finally (setf faces fs))
	      (setf (faces r) faces)))
	  ;;(setf (need-recompile r) t)
	  r)