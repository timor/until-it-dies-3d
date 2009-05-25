(asdf:defsystem until-it-dies
  :version "0"
  :description "Until It Dies - a CL 2d game engine."
  :maintainer "Josh <sykopomp@sykosomatic.org>"
  :author "Josh <sykopomp@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (cl-opengl lispbuilder-sdl lispbuilder-sdl-image lispbuilder-sdl-mixer sheeple)
  :long-description "Until It Dies is based on the code developed in Yashmup, with some improvements,
                     including opengl-graphics, and Sheeple as an object system."
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")
	     (:module util
		      :serial t
		      :components
		      ((:file "trivial-garbage")
		       (:file "opengl-hacks")
		       (:file "priority-queue")
		       (:file "utils")))
	     (:file "config")
	     (:file "resources")
	     (:file "event")
	     (:file "engine")
	     (:file "screen")
	     (:file "component")))))



