;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;simple solid body creation, lotsa stuff can be made outa that
;;TODO: improve rotary
;; give it a different name?
;; TODO: have a way of describing in the curve which curve points are smooth and which are flat
;; TODO: define =curve= and (points =curve=) so we can have bezier curves and all that stuff, too


;;DONE: compilable objects


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
		    for ns in (vertex-normals-in f) do
		    (apply #'gl:normal (coerce ns 'list))
		    (apply #'gl:vertex (coerce (point v) 'list)))
		 ))))

;;DONE: making that a reply on face and return a list of all the normals
;;DONE: use half-edgeity to only check concerning faces
;;DOING: need to check smoothity correctly 
;; TODO: debug why normals arent exactly the same sometimes 
;;      -> actually it seems they are the same in the first row of a lathed object
;;      -> in other rows they're not the same
;;       but i have no idea where things are messing up!

(defreply vertex-normal-in ((vertex =vertex=) (face =face=))
  (normalize! (apply #'vector+ (normal face) 
		     (mapcar #'normal 
			     (remove-if (lambda (nf)
					  (let ((ce (common-edge nf face)))
					    (and ce
						 (not (smoothp ce)))))
					(neighbors-at-vertex face vertex))))))

(defreply vertex-normals-in ((face =face=))
  (let (normal-faces
	(vertices (vertices face))) 
    ;;look at all faces that contain the vertex
    (loop for vertex in vertices 
       do (setf normal-faces ())
       collect
       (vertex-normal-in vertex face))))

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


;;======================bring out the lathe===============================================
(defproto =rotary= (=meshed=)
  (curve ;;=curve object
   (numsegs 5) ;;the number of sides to turn ;;TODO: rename
   ))

;;the old an working one: WHICH I AM extending to work with smooth curves now -> DONE, kinda
(defreply turn ((r =rotary=))
  (with-properties ((num-angles numsegs)) r
		   (let ((curve (points (curve r)))
			 (smoothp-list (smoothp-list (curve r))))
		     (assert (>= num-angles 4)) ;;TODO find the bug with 3 segs
		     (let* (vertices
			    faces
			    (clength (length curve))
			    (numverts (* clength num-angles))
			    (zp (first curve))
			    (np (car (last curve)))
			    (zenith (make =vertex= :point (vector 0 0 (svref zp 1))))
			    (nadir (make =vertex= :point (vector 0 0 (svref np 1))))
			    )
		       (loop 
			for vnum from 0 by clength
			for angle from 0 below (* 2 pi) by (/ (* 2 pi) num-angles)
			;;collect vertices
			append (loop 
				for ov in curve
				for ovx = (svref ov 0)
				for ovz = (svref ov 1)
				for i from 0 by 1
				collect
				(let ((new-vertex
				       (make =vertex=
					     :point (vector (* (cos angle) ovx)
							    (* (sin angle) ovx)
							    ovz))))
				  (setf (property-value new-vertex 'rotary-angle) angle
					(property-value new-vertex 'rotary-original-point) ov
					(property-value new-vertex 'rotary-point-number) (+ vnum i))
				  new-vertex)) into verts
			finally (setf vertices verts))
		       (setf vertices (append vertices (list zenith nadir)))
		       ;;second run: build up the faces
		       (loop
			with fs = ()
			for vnum from 0 by clength
			for angle from 0 below (* 2 pi) by (/ (* 2 pi) num-angles) ;;across
			do (loop
			    for i from 0 to clength
			    for j from (1-  vnum) by 1 ;;down
			    for smoothp = nil then (nth (1- i) smoothp-list)
			    do
			    (let ((new-face
				   (cond ((= i 0)
					  (make =face= :vertices
						(list
						 zenith
						 (nth (mod (+ 1 j clength) numverts) vertices)
						 (nth (1+  j) vertices)
						 )
						:neighbors fs
						:smooth-list '(t nil t)
						))
					 ((= i clength)
					  (make =face= :vertices 
						(list
						 (nth j vertices)
						 (nth (mod (+ j clength) numverts) vertices)
						 nadir)
						:neighbors fs
						:smooth-list `(,smoothp t t)
						))
					 (t
					  (make =face= :vertices
						(mapcar (fun (nth _ vertices))
							(list
							; j
							 ;(+ j 1)
							 ;(mod (+ j clength 1) numverts)
							 ;(mod (+ j clength) numverts)

							 j
							 (mod (+ j clength) numverts)
							 (mod (+ j clength 1) numverts)
							 (+ j 1)

							 ))
						:neighbors fs
						:smooth-list `(,smoothp t nil t)
						)))))
			      (setf (property-value new-face 'rotary-curve-segment) clength)
			      (push new-face fs)))
			finally (setf faces fs))
		       (setf (faces r) faces))))
  ;;(setf (need-recompile r) t)
  r)

(defreply make :after ((r =rotary=) &key)
	  (with-properties (curve) r
	    (when curve
	      (turn r))))

