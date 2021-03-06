;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;DONE: add vertex and face objects
;;DOING: neighbor messages wtf?
;;DOING: add texture coordinate property, only 2d for now, since 3d textures are waaaaaay too memory hungry

;;important: right now edge-uses that are partners must point to the same edge

(in-package #:uid)

;;=============================Vertices============================
(defproto =vertex= ()
  (point ;;these should be a vector, and a 3d one too
   color      ;;by default unset, can be an opengl color
   ;;(tex-coords #(0 0)) ;;texture coordinates, usually 2d these definitely belong to faces
   ))

(defreply print-sheeple-object ((v =vertex=) (stream =stream=))
	  (print-unreadable-object (v stream :identity t)
	    (format stream "Vertex @ ~a" (point v))))


;;=============================Edges==============================
;;edges are bot smooth by default
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
   tex-coords ;;list of texture coordinates corresponding to the vertices
   ;;kicked for now, doesnt fit the concept of materials, really:
   ;;tex-channel ;;the texture channel this face uses TODO: think about
   ))

;;perhaps not the best idea to print the verts, but this is best describing the face, after all
(defreply print-sheeple-object-verbose ((f =face=) (stream =stream=))
	  (print-unreadable-object (f stream :identity t)
	    (format stream "Face [~a~{,~a~}]"
		    (ignore-errors (point (first (vertices f))))
		    (ignore-errors (mapcar 'point (rest (vertices f)))))))

;;FIXME: this is actually geometry stuff...
(defreply normal ((face =face=))
  (apply #'3p-normal (mapcar #'point (subseq (vertices face) 0 3))))

(defreply plane ((face =face=))
  "return a plane that contains face"
  (make-plane-from-normal (normal face) (point (first (vertices face)))))


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


;;==================some construction helpers:
(defreply used-by ((vertex =vertex=) (edge =edge=))
	  (or (eq vertex (start edge))
	      (eq vertex (end edge))))

;;this one uses tolerance!
(defreply used-by ((point =vector=) (edge =edge=))
	  (assert (typep point '(vector number 3)))
	  (or (tol= point (point (start edge)))
	      (tol= point (point (end edge)))))

(defreply used-by ((vertex =vertex=) (eu =edge-use=))
	  (used-by vertex (edge eu)))

(defreply used-by ((point =vector=) (eu =edge-use=))
	  (used-by point (edge eu)))

(defreply used-by ((vertex =vertex=) (face =face=))
	  (member vertex (vertices face)))

(defreply used-by ((edge-use =edge-use=) (face =face=))
	  (member edge-use (edge-uses face)))

(defreply used-by ((edge =edge=) (face =face=))
	  (member edge (edge-uses face) :key #'edge))

;;===================makers==============
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

;;face construction, methods can be :edges :edge-uses :points or :vertices
;;TODO: throw :neighbors out, clever attaching should be used instead
;;smooth list shall be considered when creating edges
(defreply make ((proto =face=) &key edge-uses edges points vertices neighbors smooth-list)
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
							  (first vertices))))
			  ))
		   (points
		    (make proto 
			  :vertices (mapcar (fun (make =vertex= :point _))
					    points)
			  )))))
	(loop for n in neighbors
	   do (attach fresh-face n))
	(when smooth-list
	  (loop for e in (edges fresh-face)
	     for smoothp in smooth-list do
	     (setf (smoothp e) smoothp)))
	fresh-face)))

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

(defreply is-neighbor ((f1 =face=) (f2 =face=))
  (let ((n nil))
    (loop for e1 in (edges f1) do
	 (when (used-by e1 f2)
	   (setf n t)))
    n))

(defreply common-edge ((f1 =face=) (f2 =face=))
  (loop for e2 in (edges f2)
       when (used-by e2 f1)
       return e2))


;=============modifiers/helpers===========================

;;DONE: extend to same positions instead of only vertices TODO test with rotary
;;works if the same vertices are used, if more than two faces are already connected
;;edges of oldface are not touched, while edge of newface will be released if connection is found
;;DONE: need to set reversedp on new edge-use correctly
(defreply attach ((newface =face=) (oldface =face=))
	  ;;first find two vertices which both have in common
	  (let ((common-vertices (loop for v1 in (vertices newface)
				    when (member (point v1) (mapcar 'point (vertices oldface))
						 :test 'tol=)
				    collect v1)))
	    (when (= 2 (length common-vertices)) ;;only continue if this is simple (should be the case fo most convex faces)
	      (let* ((v1 (first common-vertices))
		     (v2 (second common-vertices))
		     ;;get the corresponding edge-uses on both sides
		     (old-eu (loop for eu in (edge-uses oldface)
				when (and (used-by (point v1) eu)
					  (used-by (point v2) eu))
				return eu))
		     (new-eu (loop for eu in (edge-uses newface)
				when (and (used-by (point v1) eu)
					  (used-by (point v2) eu))
				return eu)))
		;;check reversity
		(when (not (tol= (point (first (vertices old-eu)))
				 (point (first (vertices new-eu)))))
		  (setf (reversedp new-eu) t))
		;;SYNERGIZE edges and edge-uses
		(setf (edge new-eu) (edge old-eu))
		(setf (partner old-eu) new-eu)
		(setf (partner new-eu) old-eu)))))


(defreply center ((e =edge=))
	  (3p-average (start e) (end e)))

(defreply center ((f =face=))
	  (apply #'3p-average (mapcar 'point (vertices f))))

;;DONE: corner neighbours, only works on solids for now, returns nil if it detects being on an edge
;;NOT DOING: :only-smoothp <- doesnt belong here
;;TODO: extend to non-solid objects with one-partner faces
(defreply neighbors-at-vertex ((f =face=) (v =vertex=))
  "return neighbouring faces at vertex, perhaps only the ones connected with smooth edges"
	  (if (not (used-by v f))
	      (error "trying to get neighbors at a vertex that is not part of the face")
	      (loop with start-eu = (find v (edge-uses f) :key 'end)
		 for next-eu = (partner (next-in-face start-eu)) then (partner (next-in-face next-eu))
		 when (null next-eu) do (return nil)
		 until (eq next-eu start-eu)
		 collect (face next-eu)
		   )))


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
