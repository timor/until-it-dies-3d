;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;DONE: add vertex and face objects


;;important: right now edge-uses that are partners must point to the same edge

(in-package #:uid)


(defproto =vertex= ()
  (point ;;these should be a vector, and a 3d one too
   color      ;;by default unset, can be an opengl color
   ))

(defreply print-sheeple-object ((v =vertex=) (stream =stream=))
	  (print-unreadable-object (v stream :identity t)
	    (format stream "Vertex @ ~a" (point v))))

;;edges are flat by default
(defproto =edge= ()
  ;;vertices
  (start 
   end
   smoothp
   ))
(defreply print-sheeple-object ((e =edge=) (stream =stream=))
	  (print-unreadable-object (e stream :identity t)
	    (format stream (if (smoothp e)
			       "Edge ( ~a,~a )"
			       "Edge < ~a,~a >") 
		    (start e)
		    (end e))))

;;big brother has some good ideas:
(defproto =edge-use= ()
  (edge
   partner	  ;other edgeuses when connection to other face exists
   reversedp
   face ;;the face that we belong to
   ))

(defreply smoothp ((eu =edge-use=))
	  (smoothp (edge eu)))

;;only edge-uses that point to the same edge may be set as partners
(defreply (setf partner) :before (new-partner (eu =edge-use=))
	  (when (not (eq (edge new-partner)
			 (edge eu)))
	    (error "when assigned a partner, an edge-use must have the same edge as its partner")))

(defproto =face= ()
  (edge-uses ;;list of edge-uses comprising the loop
   ;;not a property, but a reply: normal
   ))

;;perhaps not the best idea to print the verts, but this is best describing the face, after all
(defreply print-sheeple-object ((f =face=) (stream =stream=))
	  (print-unreadable-object (f stream :identity t)
	    (format stream "Face [~a~{,~a~}]"
		    (point (first (vertices f)))
		    (mapcar 'point (rest (vertices f))))))

;;TODO: maybe-reversed
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

;;some construction helpers:

(defmessage used-by (usee user))

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

;;DONE: below replies should probably all be simple defuns, no?... :( ... NO!

;;KISS vertex construction
(defreply create ((proto =vertex=) &key point)
	  (check-type point (vector number *))
	  (call-next-reply proto 'point point))

;;edge construction, can be vertices or points
(defreply create ((proto =edge=) &key v1 v2 p1 p2)
	  (if (or (and v1 p1)
		  (and p2 v2))
	      (error "wanted only one out of :v1/2 or :p1/2")
	      (let ((start (if v1 v1
			       (create =vertex= :point p1)))
		    (end (if v2 v2
			     (create =vertex= :point p2))))
		(call-next-reply proto 'start start 'end end))))

;;im stupid so i have to write this helper
;;and because im even more stupid i dont actually use it...
;;returns either t or nil, meant for relative comparisons
;;loop is just a list
(defun order-in-loop (e1 e2 loop)
  (flet ((positivep (i)
	   (>= i 0)))		 ;lets really get in a fight over this
    (let ((head (car loop)))
      (when (and (eql e2 head)
		 (eql e1 (car (last loop))))
	(setf loop (append (rest loop) (list head)))
	(print 'around))
      ;;now for the positions
      (positivep (- (position e2 loop)
		    (position e1 loop))))))

;;TODO: maybe extend to same positions
;;works if the same vertices are used, if more than two faces are already connected
;;edges of oldface are not touched, while edge of newface will be released if connection is found
;;DONE: need to set reversedp on new edge-use correctly
;; TODO/DOING: write find-neighbors-among-faces to ease debugging the rotary
(defreply attach ((newface =face=) (oldface =face=))
	  ;;first find two vertices which both have in common
	  (let ((common-vertices (loop for v1 in (vertices newface)
				    when (member v1 (vertices oldface))
				    collect v1)))
	    (when (= 2 (length common-vertices)) ;;only continue if this is simple (should be the case fo most convex faces)
	      (print "found common vertices for newface:")
	      (print newface)
	      (print "oldface:")
	      (print oldface)
	      (print common-vertices)
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
;;DONE: incorporate :neighbors
(defreply create ((proto =face=) &key edge-uses edges points vertices neighbors)
	  (if (not (= 1 (count t (mapcar (fun (not (null _)))
					 (list edge-uses edges points vertices)))))
	      (error "wanted exactly one out of :edges :points :vertices :edge-uses")
	      (let ((fresh-face
		     (cond (edge-uses
			    (call-next-reply proto 'edge-uses edge-uses))
			   (edges
			    (create proto :edge-uses (mapcar (fun
							       (create =edge-use= 'edge _))
							     edges)))
			   (vertices
			    (create proto
				    :edges (loop for sublist on vertices
					      collect (create =edge=
							      :v1 (first sublist)
							      :v2 (or (second sublist)
								      (first vertices))))))
			   (points
			    (create proto :vertices (mapcar (fun (create =vertex= :point _))
							    points))))))
		(loop for n in neighbors do
		     (attach fresh-face n))
		fresh-face)))



;;make ourselves known to our edge-uses, although thats probably not good,
;;because it messes up the hierarchy and may disturb the gc, too

(defreply shared-init :after ((face =face=) &key)
	  (loop for eu in (edge-uses face)
	     do (setf (face eu) face)))
