(asdf:defsystem until-it-dies.3d
  :version "0.1 (unreleased)"
  :description "Until It Dies -- 3d stuff"
  :maintainer "tim.or@web.de"
  :author "tim.or@web.de"
  :licence "BSD-style"
  :depends-on (until-it-dies.base until-it-dies.graphics cl-glu cl-opengl alexandria)
  :components
  ((:module "src"
            :components
            ((:module "3d"
                      :components
                      (
		       (:file "export")
		       (:file "matrix")
		       (:file "util" :depends-on ("matrix"))
                       (:file "messages" :depends-on ("util"))
		       (:file "3dobject" :depends-on ("messages" "util"))
		       (:file "camera" :depends-on ("3dobject" "util"))
		       (:file "3d" :depends-on ("messages" "util" "camera" "3dobject"))
		       (:file "editor" :depends-on ("camera" "3d"))
		       (:file "compile" :depends-on ("3d"))
		       (:file "topology" :depends-on ("3d"))
		       (:file "curve" :depends-on ("topology"))
		       (:file "noise" :depends-on ("util"))
		       (:file "material" :depends-on ("noise"))
		       (:module "geometry"
				:depends-on ("3d" "topology" "compile" "curve")
				:components
				((:file "transformations")
				 (:file "meshed" :depends-on ("transformations"))
				 (:file "cuboid" :depends-on ("meshed"))
				 (:file "rotary" :depends-on ("meshed"))))))))))

(asdf:defsystem until-it-dies.3d-example
  :version "0.1 (unreleased)"
  :description "Until It Dies -- 3d stuff"
  :maintainer "tim.or@web.de"
  :author "tim.or@web.de"
  :licence "BSD-style"
  :depends-on (until-it-dies)
  :components
  ((:module "demo" :components
            ((:file "editor")))))