;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;DONE: add vertex and face objects
;;DOING: neighbor messages


;;important: right now edge-uses that are partners must point to the same edge

(in-package #:uid)

;;=============================Vertices============================
(defproto =vertex= ()
  (point ;;these should be a vector, and a 3d one too
   color      ;;by default unset, can be an opengl color
   ))

(defreply print-sheeple-object ((v =vertex=) (stream =stream=))
	  (print-unreadable-object (v stream :identity t)
	    (format stream "Vertex @ ~a" (point v))))


;;=============================Edges==============================
;;edges are flat by default
(defproto =edge= ()
  ;;vertices
  (start 
   end
   smoothp
   ))

(defreply print-sheeple-object-verbose ((e =edge=) (stream =stream=))
	  (print-unreadable-object (e stream :identity t)
	    (format stream (if (smoothp e)
			       "Edge ( ~a,~a )"
			       "Edge < ~a,~a >") 
		    (start e)
		    (end e))))

;;===========================Edge-Uses==============================

;;big brother has some good ideas:
(defproto =edge-use= ()
  (edge
   partner   ;other edgeuses when connection to other face exists
   reversedp ;wether the edge is used in the reverse direction for face
   face	     ;the face that we belong to, can this freak out the gc?
   ))

(defreply smoothp ((eu =edge-use=))
	  (smoothp (edge eu)))

;;only edge-uses that point to the same edge may be set as partners
(defreply (setf partner) :before (new-partner (eu =edge-use=))
	  (when (not (eq (edge new-partner)
			 (edge eu)))
	    (error "when assigned a partner, an edge-use must have the same edge as its partner")))

(defreply start ((eu =edge-use=))
  (if (reversedp eu)
      (end (edge eu))
      (start (edge eu))))

(defreply end ((eu =edge-use=))
  (if (reversedp eu)
      (start (edge eu))
      (end (edge eu))))


;;=====================================Faces======================================
(defproto =face= ()
  (edge-uses ;;list of edge-uses comprising the loop
   ;;not a property, but a reply: normal
   ))

;;perhaps not the best idea to print the verts, but this is best describing the face, after all
(defreply print-sheeple-object-verbose ((f =face=) (stream =stream=))
	  (print-unreadable-object (f stream :identity t)
	    (format stream "Face [~a~{,~a~}]"
		    (ignore-errors (point (first (vertices f))))
		    (ignore-errors (mapcar 'point (rest (vertices f)))))))

(defreply normal ((f =face=))
	  (apply #'3p-normal (mapcar #'point (subseq (vertices f) 0 3))))


;;hierarchy jumpers:
(defreply edges ((f =face=))
	  (mapcar #'edge (edge-uses f)))

(defreply vertices ((e =edge=))
	  (list (start e) (end e)))

(defreply vertices ((eu =edge-use=))
	  (if (reversedp eu)
	      (reverse (vertices (edge eu)))
	      (vertices (edge eu))))


;;convenience for rendering, always returns the vertices of the loop in correct order
(defreply vertices ((f =face=))
	  (loop for eu in (edge-uses f)
	     collect (first (if (reversedp eu)
			       (reverse (vertices (edge eu)))
			       (vertices (edge eu))))))

;;edge use iteration
(defreply next-in-face ((eu =edge-use=))
	  (with-properties (face) eu
	    (find (end eu) (edge-uses face) :key 'start)))
;;some construction helpers:


(defreply used-by ((vertex =vertex=) (edge =edge=))
	  (or (eq vertex (start edge))
	      (eq vertex (end edge))))

(defreply used-by ((vertex =vertex=) (eu =edge-use=))
	  (used-by vertex (edge eu)))

(defreply used-by ((vertex =vertex=) (face =face=))
	  (member vertex (vertices face)))

(defreply used-by ((edge-use =edge-use=) (face =face=))
	  (member edge-use (edge-uses face)))

(defreply used-by ((edge =edge=) (face =face=))
	  (member edge (edge-uses face) :key #'edge))


;;KISS vertex construction
(defreply make ((proto =vertex=) &key point)
	  (check-type point (vector number *))
	  (call-next-reply proto 'point point))

;;edge construction, can be vertices or points
(defreply make ((proto =edge=) &key v1 v2 p1 p2)
	  (if (or (and v1 p1)
		  (and p2 v2))
	      (error "wanted only one out of :v1/2 or :p1/2")
	      (let ((start (if v1 v1
			       (make =vertex= :point p1)))
		    (end (if v2 v2
			     (make =vertex= :point p2))))
		(call-next-reply proto 'start start 'end end))))

;;TODO: extend to same positions instead of only vertices
;;works if the same vertices are used, if more than two faces are already connected
;;edges of oldface are not touched, while edge of newface will be released if connection is found
;;DONE: need to set reversedp on new edge-use correctly
(defreply attach ((newface =face=) (oldface =face=))
	  ;;first find two vertices which both have in common
	  (let ((common-vertices (loop for v1 in (vertices newface)
				    when (member v1 (vertices oldface))
				    collect v1)))
	    (when (= 2 (length common-vertices)) ;;only continue if this is simple (should be the case fo most convex faces)
	      (let* ((v1 (first common-vertices))
		     (v2 (second common-vertices))
		     ;;get the corresponding edge-uses on both sides
		     (old-eu (loop for eu in (edge-uses oldface)
				when (and (used-by v1 eu)
					  (used-by v2 eu))
				return eu))
		     (new-eu (loop for eu in (edge-uses newface)
				when (and (used-by v1 eu)
					  (used-by v2 eu))
				return eu)))
		;;check reversity
		(when (not (eql (first (vertices old-eu))
				(first (vertices new-eu))))
		  (setf (reversedp new-eu) t))
		;;SYNERGIZE edges and edge-uses
		(setf (edge new-eu) (edge old-eu))
		(setf (partner old-eu) new-eu)
		(setf (partner new-eu) old-eu)))))

;;face construction, methods can be :edges :edge-uses :points or :vertices
;;TODO: throw :neighbors out, clever attaching should be used instead
(defreply make ((proto =face=) &key edge-uses edges points vertices neighbors)
	  (if (not (= 1 (count t (mapcar (fun (not (null _)))
					 (list edge-uses edges points vertices)))))
	      (error "wanted exactly one out of :edges :points :vertices :edge-uses")
	      (let ((fresh-face
		     (cond (edge-uses
			    (call-next-reply proto 'edge-uses edge-uses))
			   (edges
			    (make proto :edge-uses (mapcar (fun
							       (make =edge-use= 'edge _))
							     edges)))
			   (vertices
			    (make proto
				    :edges (loop for sublist on vertices
					      collect (make =edge=
							      :v1 (first sublist)
							      :v2 (or (second sublist)
								      (first vertices))))))
			   (points
			    (make proto :vertices (mapcar (fun (make =vertex= :point _))
							    points))))))
		(loop for n in neighbors
		     do (attach fresh-face n))
		fresh-face)))

(defreply center ((f =face=))
	  (apply #'3p-average (mapcar 'point (vertices f))))


;;make ourselves known to our edge-uses, although thats probably not good,
;;because it messes up the hierarchy and may disturb the gc, too
;;wouldnt be a linked half-edge list otherwise, though
(defreply shared-init :after ((face =face=) &key)
	  (loop for eu in (edge-uses face)
	     do (setf (face eu) face)))

(defreply neighbors ((f =face=))
  "return all the faces that share edges, in order"
  (loop for eu in (edge-uses f)
       when (partner eu)
       collect (face (partner eu))))


;;DONE: corner neighbours, only works on solids for now, invalid on "edges" of a constellation
(defreply neighbors-at-vertex ((f =face=) (v =vertex=))
  (if (not (used-by v f))
      (error "trying to get neighbors at a vertex that is not part of the face")
      (loop with start-eu = (find v (edge-uses f) :key 'end)
	   for next-eu = (partner (next-in-face start-eu)) then (partner (next-in-face next-eu))
	   collect (face next-eu)
	   until (or (null next-eu)
		     (eq next-eu start-eu)))))