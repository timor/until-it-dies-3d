;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage until-it-dies
  (:nicknames :uid)
  (:use :cl :sheeple)
  (:shadow :step)
  (:export

   ;; time
   :now
   :time-difference
   ;; text
   :out
   :build-string

   ;; core
   :init
   :teardown
   :run
   :update
   :draw

   :attach
   :detach
   :detach-all

   ;; events
   :key-up
   :key-down
   :key-down-p
   :mouse-up
   :mouse-down
   :mouse-move
   :joystick-button-down
   :joystick-button-up
   :joystick-move
   :window-resized
   :step-engine

   ;; joysticks
   :=joystick=
   :joystick-number
   :num-axes
   :num-buttons
   :axis-positions
   :button-states

   ;; Engine
   :=engine=
   :*engine*
   :make-engine
   :with-engine
   :runningp
   :initializedp
   :dt
   :event-queue
   :resource-manager
   :clear-color
   :pausedp
   :resizablep
   :windowedp
   :key-repeat-p
   :mouse-visible-p
   :mouse-x
   :mouse-y
   :joysticks
   :window-width
   :window-height
   :current-view
   :title
   :fps
   :quit
   :cumulative-mean-fps
   :last-fps
   :mean-fps

   ;; Views
   :=view=
   :view-left
   :view-right
   :view-bottom
   :view-top
   :view-far
   :view-near
   :make-view

   ;; primitives
   :make-color
   :mix-colors
   :red
   :green
   :blue
   :alpha
   :*color*
   :with-color
   :*black*
   :*white*
   :*magenta*
   :*red*
   :*green*
   :*blue*
   :*yellow*
   :*orange*
   :*brown*
   :make-point
   :point-x
   :point-y
   :point-z
   :draw-rectangle
   :draw-circle
   :draw-triangle
   :draw-quad
   :draw-point
   :draw-points
   :draw-line
   :draw-polygon

   ;; resources
   :=resource=
   :=resource-manager=
   :*resource-manager*
   :=file-resource=
   :load-resource
   :unload-resource
   :loadedp

   ;; textures
   :=texture=
   :=file-texture=
   :bind-texture
   :unbind-texture
   :make-texture

   ;; sounds
   :=sound=
   :=file-sound=
   :make-sound
   :source-position
   :source-velocity
   :source-direction
   :sound-state
   :play
   :stop
   :pause
   :rewind

   ;; fonts
   :=font=
   :make-font
   :*font*
   :with-font
   :size
   :res

   ;; sprites
   :draw
   :draw-at
   :make-image
   :width
   :height
   :filepath
   :make-animation
   :num-frames
   :frame-delay
   :frame-width
   :frame-height
   :animation-type

   ;; events
   :*event-queue*
   :fork
   :with-event-queue
   ))
