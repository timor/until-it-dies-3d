;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-


;;TODO: improve rotary
;; give it a different name?
;; DONE: have a way of describing in the curve which curve points are smooth and which are flat
;; DONE: define =curve= and (points =curve=) so we can have bezier curves and all that stuff, too
;; TODO: get that (O)! out by knowing what to attach where
;; TODO: specialize draw to use triangle-strips and -fans, since they are fastest


;;TODO: surface objects -> somewhere else


(in-package #:uid)

;;======================bring out the lathe===============================================
(defproto =rotary= (=meshed=)
  (curve ;;=curve= object
   (sides 5) ;;the number of sides used to approximate curve shape
   ))

;;the old an working one: WHICH I AM extending to work with smooth curves now -> DONE, kinda
(defreply turn ((r =rotary=))
  (with-properties ((num-angles sides)) r
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
				   (cond ((= i 0) ;face with zenith
					  (make =face= :vertices
						(list
						 zenith
						 (nth (mod (+ 1 j clength) numverts) vertices)
						 (nth (1+  j) vertices)
						 )
						:neighbors fs
						:smooth-list '(t nil t)
						))
					 ((= i clength) ;face with nadir
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

(defreply shared-init :after ((r =rotary=) &key)
	  (with-properties (curve) r
	    (when curve
	      (turn r))))

