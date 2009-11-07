;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(cl:defpackage until-it-dies.resource-info
  (:export :*resource-directory*))

(cl:defvar until-it-dies.resource-info:*resource-directory*
  (print (merge-pathnames "res/" *load-truename*)))

(asdf:defsystem until-it-dies
  :version "0.1 (unreleased)"
  :description "Until It Dies -- A 2D Game Engine."
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (sheeple cl-opengl cl-openal cl-glfw)
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
                      ((:file "opengl-hacks")
                       (:file "priority-queue")
                       (:file "utils")))
             (:file "input")
             (:file "messages")
             (:file "colors")
             (:file "primitives")
             (:file "config")
             (:file "event")
             (:module resources
                      :serial t
                      :components
                      ((:file "finalizers")
                       (:file "resources")
                       (:file "devil")
                       (:file "textures")
                       (:file "ftgl")
                       (:file "fonts")
                       (:file "sounds")))
             (:file "sprite")
             (:file "engine")))))

(asdf:defsystem until-it-dies.examples
  :version "0.1 (unreleased)"
  :description "Examples for Until It Dies -- A 2D Game Engine"
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies)
  :components
  ((:module demo
            :components
            ((:file "testgame")))))
