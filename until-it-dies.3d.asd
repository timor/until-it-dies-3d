(asdf:defsystem until-it-dies.3d
  :version "0.1 (unreleased)"
  :description "Until It Dies -- 3d stuff"
  :maintainer "tim.or@web.de"
  :author "tim.or@web.de"
  :licence "BSD-style"
  :depends-on (until-it-dies.base cl-glu cl-opengl alexandria)
  :components
  ((:module "src"
            :components
            ((:module "3d"
                      :components
                      ((:file "util")
                       (:file "messages")
		       (:file "3dobject" )
		       (:file "camera" :depends-on ("3dobject"))
		       (:file "3d" :depends-on ("messages" "util" "camera" "3dobject"))
		       (:file "compile" :depends-on ("3d"))
		       (:file "topology" :depends-on ("3d"))
		       (:file "solid" :depends-on ("3d" "topology" "compile"))
		       ))))))
