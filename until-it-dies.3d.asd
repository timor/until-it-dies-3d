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
                      (
		       (:file "export")
		       (:file "util")
                       (:file "messages" :depends-on ("util"))
		       (:file "3dobject" :depends-on ("messages" "util"))
		       (:file "camera" :depends-on ("3dobject" "util"))
		       (:file "3d" :depends-on ("messages" "util" "camera" "3dobject"))
		       (:file "compile" :depends-on ("3d"))
		       (:file "topology" :depends-on ("3d"))
		       (:file "curve" :depends-on ("topology"))
		       (:file "solid" :depends-on ("3d" "topology" "compile" "curve"))
		       (:file "noise" :depends-on ("util"))
		       ))))))

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