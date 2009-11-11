(asdf:defsystem until-it-dies.graphics
  :version "0.1 (unreleased)"
  :description "Until It Dies -- Fancy graphics module."
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies.base)
  :components
  ((:module "src"
            :components
            ((:module "graphics"
                      :components
                      ((:file "devil")
                       (:file "textures" :depends-on ("devil" "resources"))
                       (:file "ftgl")
                       (:file "fonts" :depends-on ("ftgl" "resources"))
                       (:file "sprite" :depends-on ("messages" "resources"))))))))
