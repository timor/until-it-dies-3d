;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;TODO: add vertex and face objects

(in-package #:uid)


(defproto =vertex= ()
  (point ;;these should be a vector, and a 3d one too
   color      ;;by default unset, can be an opengl color
   ))

;;edges are flat by default, can be overridden per face
(defproto =edge= ()
  ;;vertices
  (start 
   end
   smoothp
   ))

;;big brother has some good ideas:
(defproto =edge-use= ()
  (edge
   partner	  ;other edgeuses when connection to other face exists
   reversedp
   ))

(defproto =face= ()
  (edge-uses ;;list of edge-uses comprising the loop
   ))

;;convenience for rendering, always returns the vertices of the loop in correct order
(defreply vertices ((f =face=))
	  (loop for eu in (edge-uses f)
	       append (if (reversedp eu)
			  (reverse (vertices (edge eu)))
			  (vertices (edge eu)))))


;;some construction helpers:

(defmessage used-by (usee user))

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

;;face construction, methods can be :edges :edge-uses :points or :vertices
(defreply create ((proto =face=) &key edge-uses edges points vertices)
	  (if (not (= 1 (count t (mapcar (fun (not (null _)))
					 (list edge-uses edges points vertices)))))
	      (error "wanted exactly one out of :edges :points :vertices :edge-uses")
	      (cond (edge-uses
		     (call-next-reply proto 'edge-uses edge-uses))
		    (edges
		     (create proto :edge-uses (mapcar (fun
							(create =edge-use= 'edge _))
						      edges)))
		    (vertices
		     (create proto
			     :edges (maplist (lambda (sublist)
					       (create =edge=
						       :v1 (first sublist)
						       :v2 (or (second sublist)
							       (first vertices))))
					     vertices)))
		    (points
		     (create proto :vertices (mapcar (fun (create =vertex= :point _))
						     points))))))