;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:uid)

(export '(
	  ;;utility
	  3dp
	  3p-normal
	  vector-between
	  vector+
	  3p-average
	  normalize!
	  ;;this is in uid package file: clamp
	  tol=
	  *tolerance*
	  printv
	  
	  ;;defmessages
	  draw-3d
	  draw-2d
	  add-content
	  remove-content
	  print-sheeple-object-verbose
	  compile-display
	  schedule-recompile
	  turn
	  used-by
	  normal
	  next-in-face
	  attach
	  neighbors

	  =container=
	  hooks
	  
	  =3dsheep=
	  current-3dview
	  2d-content
	  3d-content
	  run-in-thread
	  clear

	  =3dobject=
	  x
	  y
	  z
	  move-to

	  =3dview=
	  eye
	  center
	  up
	  fov
	  aspect
	  =camera=
	  change-view-to-camera
	  orbit-by
	  
	  =vertex=
	  point
	  color

	  =edge=
	  start
	  end
	  smoothp

	  ;;this is probably not a thing to export
	  ;;=edge-use=
	  
	  =face=
	  vertices
	  next-in-face
	  is-neighbor
	  ;;redundant: center

	  =compilable=
	  locks
	  schedule-recompile
	  recompile-all
	  compile-display

	  =curve=
	  curve-points
	  smoothp-list
	  point-normals

	  =meshed=
	  edges
	  auto-normals ;;TODO: implement :D

	  =rotary=
	  ;;turn see above
	  curve
	  numsegs
	  
	  ))