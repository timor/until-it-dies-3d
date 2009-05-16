(asdf:defsystem uid
  :version "0"
  :description "Shoot It Until It Dies"
  :maintainer "Josh <sykopomp@Dagon>"
  :author "Josh <sykopomp@Dagon>"
  :licence "BSD-style"
  :depends-on (cl-opengl lispbuilder-sdl lispbuilder-sdl-image lispbuilder-sdl-mixer sheeple)
  :long-description "SIUID is mostly just a rewrite of Yashmup, but Sheeple-based, 
                     and with an opengl drawing engine"
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")
	     (:module util
		      :serial t
		      :components
		      ((:file "utils")
		       (:file "priority-queue")))
	     (:module opengl
		      :serial t
		      :components
		      ((:file "opengl")
		       (:file "texture")
		       (:file "particles")))
	     (:file "config")
	     (:file "game")))))


