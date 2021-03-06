(defpackage #:uid-glfw-types
  (:use #:cl #:cffi)
  (:shadow #:boolean #:byte #:float #:char #:string)
  (:export #:enum #:boolean #:bitfield #:byte #:short #:int #:sizei #:ubyte #:ushort #:uint
           #:float #:clampf #:double #:clampd #:void #:uint64 #:int64
           #:intptr #:sizeiptr
           #:handle
           #:char #:string
           #:half))

(in-package #:uid-glfw-types)

(defctype enum :uint32)
(defctype boolean :uint8)
(defctype bitfield :uint32)
(defctype byte :int8)
(defctype short :int16)
(defctype int :int32)
(defctype sizei :int32)
(defctype ubyte :uint8)
(defctype ushort :uint16)
(defctype uint :uint32)
(defctype float :float)
(defctype clampf :float)
(defctype double :double)
(defctype clampd :double)
(defctype void :void)

#-cffi-features:no-long-long
(defctype uint64 :uint64)
#-cffi-features:no-long-long
(defctype int64 :int64)

;; Find a CFFI integer type the same foreign-size as a pointer
(defctype intptr #.(find-symbol (format nil "INT~d" (* 8 (cffi:foreign-type-size :pointer)))
                                (find-package '#:keyword)))
(defctype sizeiptr #.(find-symbol (format nil "INT~d" (* 8 (cffi:foreign-type-size :pointer)))
                                  (find-package '#:keyword)))

(defctype handle :unsigned-int)

(defctype char :char)

(defctype string :string)

(defctype half :unsigned-short) ; this is how glext.h defines it anyway

(defmethod cffi:expand-to-foreign (value (type (eql 'boolean)))
  `(if ,value 1 0))

(defmethod cffi:expand-from-foreign (value (type (eql 'boolean)))
  `(not (= ,value 0)))

(defmethod cffi:expand-to-foreign (value (type (eql 'clampf)))
  `(coerce ,value 'single-float))

(defmethod cffi:expand-to-foreign (value (type (eql 'clampd)))
  `(coerce ,value 'double-float))

(defmethod cffi:expand-to-foreign (value (type (eql 'float)))
  `(coerce ,value 'single-float))

(defmethod cffi:expand-to-foreign (value (type (eql 'double)))
  `(coerce ,value 'double-float))

;; TODO: Maybe we can find/write a converter to a half? Does anyone want it?
;; TODO: Might we want converters to integer types? What would it be? round, or floor (or even ceil)?

(defpackage uid-glfw
  (:use :cl :cffi :uid-glfw-types)
  (:shadowing-import-from #:uid-glfw-types #:boolean #:byte #:float #:char #:string)
  (:shadow #:+red-bits+ #:+green-bits+ #:+blue-bits+
           #:+alpha-bits+ #:+stencil-bits+ #:+depth-bits+
           #:+accum-red-bits+ #:+accum-green-bits+ #:+accum-blue-bits+
           #:+accum-alpha-bits+ #:+aux-buffers+ #:+stereo+
           #:enable #:disable)
  (:export #:key/button-state #:key #:+accelerated+ #:+accum-alpha-bits+ #:+accum-blue-bits+
           #:+accum-green-bits+ #:+accum-red-bits+ #:+active+ #:+alpha-bits+
           #:+alpha-map-bit+ #:+auto-poll-events+ #:+aux-buffers+ #:+axes+
           #:+blue-bits+ #:+build-mipmaps-bit+ #:+buttons+ #:+depth-bits+ #:+false+
           #:+fsaa-samples+ #:+fullscreen+ #:+green-bits+ #:+iconified+ #:+infinity+
           #:+mouse-button-1+ #:+mouse-button-2+ #:+mouse-button-3+ #:+mouse-button-4+
           #:+mouse-button-5+ #:+mouse-button-6+ #:+mouse-button-7+ #:+mouse-button-8+
           #:+mouse-button-last+ #:+mouse-button-left+ #:+mouse-button-middle+
           #:+mouse-button-right+ #:+mouse-cursor+ #:+no-rescale-bit+ #:+nowait+ #:+opened+
           #:+origin-ul-bit+ #:+present+ #:+press+ #:+red-bits+ #:+refresh-rate+ #:+release+
           #:+stencil-bits+ #:+stereo+ #:+sticky-keys+ #:+sticky-mouse-buttons+ #:+system-keys+
           #:+true+ #:+wait+ #:+window+ #:+window-no-resize+ #:boolean #:broadcast-cond
           #:close-window #:defcfun+doc #:defcfun+out+doc #:disable #:do-window #:enable
           #:extension-supported #:free-image #:get-desktop-mode #:get-gl-version
           #:get-joystick-buttons #:get-joystick-param #:get-joystick-pos #:get-key
           #:get-mouse-button #:get-mouse-pos #:get-mouse-wheel #:get-proc-address
           #:get-version #:get-video-modes #:get-window-param #:get-window-size #:iconify-window
           #:init #:load-memory-texture-2d #:load-texture-2d #:load-texture-image-2d #:open-window
           #:open-window-hint #:poll-events #:read-image #:read-memory-image #:restore-window
           #:set-char-callback #:set-key-callback #:set-mouse-button-callback #:set-mouse-pos
           #:set-mouse-pos-callback #:set-mouse-wheel #:set-mouse-wheel-callback #:set-time
           #:set-window-close-callback #:set-window-pos #:set-window-refresh-callback #:set-window-size
           #:set-window-size-callback #:set-window-title #:swap-buffers #:swap-interval #:terminate
           #:translate-control-key #:with-init #:with-init-window #:with-lock-mutex #:with-open-window))
(in-package #:uid-glfw)

(defmacro defcfun+out+doc ((c-name lisp-name) return-type (&body args))
  (let ((internal-name (intern (format nil "%~a" lisp-name)))
        (in-arg-names (mapcar #'second (remove-if-not #'(lambda (arg)
                                                          (eql (car arg) :in))
                                                      args)))
        (out-args (mapcar #'cdr (remove-if-not #'(lambda (arg)
                                                   (eql (car arg) :out))
                                               args))))
    `(progn
       (defcfun (,c-name ,internal-name) ,return-type
         ,@(mapcar #'(lambda (arg)
                       (if (eql (car arg) :out)
                           (list (second arg) :pointer)
                           (cdr arg)))
                   args))
       (defun ,lisp-name ,in-arg-names
         (with-foreign-objects ,out-args
           (,internal-name ,@(mapcar #'second args))
           (list ,@(mapcar #'(lambda (arg)
                               `(mem-ref ,(first arg) ',(second arg)))
                           out-args)))))))

;; ECL's DFFI seems to have issues if you don't put the full path in
#+(and unix ecl)
(setf cffi:*foreign-library-directories*
      (list "/usr/local/lib/" "/usr/lib/"))

(cffi:load-foreign-library '(:or
                             #+darwin (:framework "GLFW")
                             (:default "glfw")
                             (:default "libglfw")))

;; Key and button state/action definitions
(defcenum key/button-state
  :release :press)

;; Keyboard key definitions: 8-bit ISO-8859-1 (Latin 1) encoding is used
;; for printable keys (such as A-Z, 0-9 etc), and values above 256
;; represent special (non-printable) keys (e.g. F1, Page Up etc).
(cffi:defcenum key

  (:unknown -1) (:space 32) (:special 256) :escape

  :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13
  :f14 :f15 :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :f25

  :up :down :left :right

  :left-shift :right-shift :left-ctrl :right-ctrl :left-alt :right-alt

  :tab :enter :backspace

  :insert :delete :page-up :page-down :home :end

  :keypad-0 :keypad-1 :keypad-2 :keypad-3 :keypad-4
  :keypad-5 :keypad-6 :keypad-7 :keypad-8 :keypad-9
  :keypad-divide :keypad-multiply :keypad-subtract :keypad-add
  :keypad-decimal :keypad-equal :keypad-enter

  (:last #.(+ 256 62)))

;; Mouse button definitions
(defcenum mouse-button
  :mouse-button-1
  :mouse-button-2
  :mouse-button-3
  :mouse-button-4
  :mouse-button-5
  :mouse-button-6
  :mouse-button-7
  :mouse-button-8
  :mouse-button-last
  ;; Mouse button aliases
  (:mouse-button-left 0)
  :mouse-button-right
  :mouse-button-middle)

;;========================================================================
;; Other definitions
;;========================================================================

(defcenum open-window-mode
  ;; glfwOpenWindow modes
  (:window #x00010001)
  (:fullscreen #x00010002))

;; glfwGetWindowParam tokens
(defcenum window-param
  (:opened #x00020001)
  (:active #x00020002)
  (:iconified #x00020003)
  (:accelerated #x00020004)
  (:red-bits #x00020005)
  (:green-bits #x00020006)
  (:blue-bits #x00020007)
  (:alpha-bits #x00020008)
  (:depth-bits #x00020009)
  (:stencil-bits #x0002000a)

;; The following constants are used for both glfwGetWindowParam
;; and glfwOpenWindowHint
  (:refresh-rate #x0002000b)
  (:accum-red-bits #x0002000c)
  (:accum-green-bits #x0002000d)
  (:accum-blue-bits #x0002000e)
  (:accum-alpha-bits #x0002000f)
  (:aux-buffers #x00020010)
  (:stereo #x00020011)
  (:window-no-resize #x00020012)
  (:fsaa-samples #x00020013))

;; glfwEnable/glfwDisable tokens
(defcenum enable/disable
  (:mouse-cursor #x00030001)
  (:sticky-keys #x00030002)
  (:sticky-mouse-buttons #x00030003)
  (:system-keys #x00030004)
  (:key-repeat #x00030005)
  (:auto-poll-events #x00030006))

;; glfwGetJoystickParam tokens
(defcenum joystick-param
  (:present #x00050001)
  (:axes #x00050002)
  (:buttons #x00050003))

(defcfun ("glfwInit" init) boolean)

(defcfun ("glfwTerminate" terminate) :void)

(defcfun+out+doc ("glfwGetVersion" get-version) :void
  ((:out major :int) (:out minor :int) (:out rev :int)))

(defmacro without-fp-traps (&body body)
  `(#+ (and sbcl x86-64) sb-int:with-float-traps-masked
       #+ (and sbcl x86-64)(:invalid :divide-by-zero)
    #- (and sbcl x86-64) progn
     ,@body))

(defmacro with-init (&body forms)
  "Call uid-glfw:init, execute forms and clean-up with uid-glfw:terminate once finished.
This makes a nice wrapper to an application higher-level form.
Signals an error on failure to initialize. Wrapped in a block named uid-glfw:with-init."
  `(without-fp-traps
     (if (uid-glfw:init)
        (unwind-protect
             (block with-init ,@forms)
          (uid-glfw:terminate))
        (error "Error initializing glfw."))))

(defcfun ("glfwOpenWindow" %open-window) boolean
  (width :int) (height :int)
  (redbits :int) (greenbits :int) (bluebits :int) (alphabits :int)
  (depthbits :int) (stencilbits :int) (mode open-window-mode))

(declaim (inline open-window))
(defun open-window (width height &key (redbits 0) (greenbits 0) (bluebits 0)
                    (alphabits 0) (depthbits 0) (stencilbits 0) (mode :window))
  (%open-window width height redbits greenbits bluebits alphabits depthbits stencilbits mode))

(defcfun ("glfwOpenWindowHint" %open-window-hint) :void (target window-param) (hint :int))
(defun open-window-hint (target hint)
  (case target
    ((:window-no-resize :stereo)
     (%open-window-hint target (if hint 1 0)))
    (otherwise
     (%open-window-hint target hint))))

(defcfun ("glfwCloseWindow" close-window) :void)

(defmacro with-open-window ((title width height &key (redbits 0) (greenbits 0) (bluebits 0)
                                   (alphabits 0) (depthbits 0) (stencilbits 0) (mode :window))
                            &body forms)
  `(if (%open-window ,width ,height ,redbits ,greenbits ,bluebits
                     ,alphabits ,depthbits ,stencilbits ,mode)
       (unwind-protect
            (block with-open-window
              (uid-glfw:set-window-title ,title)
              ,@forms)
         (terminate))
       (error "Error initializing glfw window.")))

(defcfun ("glfwSetWindowCloseCallback" set-window-close-callback) :void (cbfun :pointer))

(defcfun ("glfwSetWindowTitle" set-window-title) :void (title :string))

(defcfun ("glfwSetWindowSize" set-window-size) :void (width :int) (height :int))

(defcfun ("glfwSetWindowPos" set-window-pos) :void (x :int) (y :int))

(defcfun ("glfwGetWindowSize" %get-window-size) :void (width :pointer) (height :pointer))
(defun get-window-size ()
  (cffi:with-foreign-objects ((width :int)
                              (height :int))
    (%get-window-size width height)
    (list (mem-ref width :int)
          (mem-ref height :int))))

(defcfun ("glfwSetWindowSizeCallback" set-window-size-callback) :void (cbfun :pointer))

(defcfun ("glfwIconifyWindow" iconify-window) :void)

(defcfun ("glfwRestoreWindow" restore-window) :void)

(defcfun ("glfwGetWindowParam" %get-window-param) :int (param window-param))
(defun get-window-param (param)
  (case param
    ((:opened :active :iconified :accelerated :window-no-resize :stereo)
     (when (= 1 (%get-window-param param)) t))
    (otherwise (%get-window-param param))))

(defcfun ("glfwSwapBuffers" swap-buffers) :void)

(defcfun ("glfwSwapInterval" swap-interval) :void (interval :int))

(defcfun ("glfwSetWindowRefreshCallback" set-window-refresh-callback) :void (cbfun :pointer))

(defcstruct vidmode
  (width :int)
  (height :int)
  (redbits :int)
  (bluebits :int)
  (greenbits :int))

(defcfun ("glfwGetVideoModes" %get-video-modes) :int (list :pointer) (maxcount :int))

(defun get-video-modes (maxcount)
  (with-foreign-object (list 'vidmode maxcount)
    (let ((count (%get-video-modes list maxcount)))
      (loop for i below count
         collecting
         (let ((mode (cffi:mem-aref list 'vidmode i)))
           (list (foreign-slot-value mode 'vidmode 'width)
                 (foreign-slot-value mode 'vidmode 'height)
                 (foreign-slot-value mode 'vidmode 'redbits)
                 (foreign-slot-value mode 'vidmode 'greenbits)
                 (foreign-slot-value mode 'vidmode 'bluebits)))))))

(defcfun ("glfwGetDesktopMode" %get-desktop-mode) :void (mode :pointer))
(defun get-desktop-mode ()
  (with-foreign-object (mode 'vidmode)
    (%get-desktop-mode mode)
    (list (foreign-slot-value mode 'vidmode 'width)
          (foreign-slot-value mode 'vidmode 'height)
          (foreign-slot-value mode 'vidmode 'redbits)
          (foreign-slot-value mode 'vidmode 'greenbits)
          (foreign-slot-value mode 'vidmode 'bluebits))))

(defcfun ("glfwGetKey" get-key) key/button-state (key :int))

(defun translate-control-key (key)
  (foreign-enum-keyword 'key key))

(defcfun ("glfwGetMouseButton" get-mouse-button) key/button-state (button mouse-button))

(defcfun+out+doc ("glfwGetMousePos" get-mouse-pos) :void ((:out xpos :int) (:out ypos :int)))

(defcfun ("glfwSetMousePos" set-mouse-pos) :void (xpos :int) (ypos :int))

(defcfun ("glfwGetMouseWheel" get-mouse-wheel) :int)

(defcfun ("glfwSetMouseWheel" set-mouse-wheel) :void (pos :int))

(defcfun ("glfwSetKeyCallback" set-key-callback) :void (cbfun :pointer))
(defcfun ("glfwSetCharCallback" set-char-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMouseButtonCallback" set-mouse-button-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMousePosCallback" set-mouse-pos-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMouseWheelCallback" set-mouse-wheel-callback) :void (cbfun :pointer))

(defcfun ("glfwGetJoystickParam" %get-joystick-param) :int (joy :int) (param joystick-param))
(defun get-joystick-param (joystick param)
  (if (eq :present param)
      (cffi:translate-from-foreign (%get-joystick-param joystick param) 'boolean)
      (%get-joystick-param joystick param)))

(defcfun ("glfwGetJoystickPos" %get-joystick-pos) :int (joy :int) (pos :pointer) (numaxes :int))

(defun get-joystick-pos (joy numaxes)
  (with-foreign-object (pos :float numaxes)
    (let ((numaxes (%get-joystick-pos joy pos numaxes)))
      (loop for i below numaxes collecting (mem-aref pos :float i)))))

(defcfun ("glfwGetJoystickButtons" %get-joystick-buttons) :int (joy :int) (buttons :pointer) (numbuttons :int))
(defun get-joystick-buttons (joy numbuttons)
  (with-foreign-object (buttons :unsigned-char numbuttons)
    (let ((numbuttons (%get-joystick-buttons joy buttons numbuttons)))
      (loop for i below numbuttons collecting (mem-aref buttons :unsigned-char i)))))

(defcfun ("glfwExtensionSupported" extension-supported) boolean (extension :string))

(defcfun ("glfwGetProcAddress" get-proc-address) :pointer (procname :string))

(defcfun+out+doc ("glfwGetGLVersion" get-gl-version) :void
  ((:out major :int) (:out minor :int) (:out rev :int)))

(defcfun ("glfwEnable" enable) :void (token enable/disable))
(defcfun ("glfwDisable" disable) :void (token enable/disable))
