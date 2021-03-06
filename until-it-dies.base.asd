;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem until-it-dies.base
  :version "0.1 (unreleased)"
  :description "Until It Dies -- A 2D Game Engine."
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (sheeple cl-opengl)
  :serial t
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:module "util" :depends-on ("packages")
                      :components
                      ((:file "opengl-hacks")
                       (:file "priority-queue")
                       (:file "glfw")
                       (:file "utils")))
             (:module "resources" :depends-on ("util" "messages")
                      :components
                      ((:file "finalizers")
                       (:file "resources" :depends-on ("finalizers"))))
             (:file "messages" :depends-on ("util"))
             (:file "input" :depends-on ("messages"))
             (:file "colors" :depends-on ("util"))
             (:file "primitives" :depends-on ("colors"))
             (:file "event" :depends-on ("messages"))
             (:file "view" :depends-on ("messages"))
             (:file "engine" :depends-on ("event" "view" "input" "util" "colors" "resources"))))))

(asdf:defsystem until-it-dies.examples
  :version "0.1 (unreleased)"
  :description "Examples for Until It Dies -- A 2D Game Engine"
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies)
  :components
  ((:module "demo" :components
            ((:file "testgame")))))
