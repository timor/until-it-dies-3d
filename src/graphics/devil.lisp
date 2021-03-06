(defpackage uid-il
  (:use :cl :cffi)
  (:export :init :shutdown :get-integer :bind-image :get-error))
(in-package :uid-il)

(define-foreign-library il
  (:unix (:or "libIL" "libIL.so.1"))
  (:windows "DevIL.dll")
  (t (:default "libIL")))
(use-foreign-library il)
(defcenum mode
  (:file-overwrite #x0620)
  (:file-mode #x0621)
  (:conv-pal #x0630)
  (:use-key-color #x0635)
  (:png-alpha-index #x0724)
  (:version-num #x0DE2)
  (:image-width #x0DE4)
  (:image-height #x0DE5)
  (:image-depth #x0DE6)
  (:image-size-of-data #x0DE7)
  (:image-bpp #x0DE8)
  (:image-bytes-per-pixel #x0DE8)
  (:image-bits-per-pixel #x0DE9)
  (:image-format #x0DEA)
  (:image-type #x0DEB)
  (:palette-type #x0DEC)
  (:palette-size #x0DED)
  (:palette-bpp #x0DEE)
  (:palette-num-cols #x0DEF)
  (:palette-base-type #x0DF0)
  (:num-images #x0DF1)
  (:num-mipmaps #x0DF2)
  (:num-layers #x0DF3)
  (:active-image #x0DF4)
  (:active-mipmap #x0DF5)
  (:active-layer #x0DF6)
  (:cur-image #x0DF7)
  (:image-duration #x0DF8)
  (:image-planesize #x0DF9)
  (:image-bpc #x0DFA)
  (:image-offx #x0DFB)
  (:image-offy #x0DFC)
  (:image-cubeflags #x0DFD)
  (:image-origin #x0DFE)
  (:image-channels #x0DFF))

(defcenum error
  (:no-error #x0000)
  (:invalid-enum #x0501)
  (:out-of-memory #x0502)
  (:format-not-supported #x0503)
  (:internal-error #x0504)
  (:invalid-value #x0505)
  (:illegal-operation #x0506)
  (:illegal-file-value #x0507)
  (:invalid-file-header #x0508)
  (:invalid-param #x0509)
  (:could-not-open-file #x050A)
  (:invalid-extension #x050B)
  (:file-already-exists #x050C)
  (:out-format-same #x050D)
  (:stack-overflow #x050E)
  (:stack-underflow #x050F)
  (:invalid-conversion #x0510)
  (:bad-dimensions #x0511)
  (:file-read-error #x0512)
  (:file-write-error #x0512)
  (:lib-gif-error #x05E1)
  (:lib-jpeg-error #x05E2)
  (:lib-png-error #x05E3)
  (:lib-tiff-error #x05E4)
  (:lib-mng-error #x05E5)
  (:unknown-error #x05FF))

(defcfun ("ilInit" init) :void)
(defcfun ("ilShutDown" shutdown) :void)
(defcfun ("ilGetInteger" get-integer) :uint (mode mode))
(defcfun ("ilBindImage" bind-image) :void (image :int))
(defcfun ("ilGetError" get-error) error)

(defpackage uid-ilut
  (:use :cl :cffi)
  (:export :init :renderer :enable :disable :gl-load-image))
(in-package :uid-ilut)

(define-foreign-library ilut
  (:unix (:or "libILUT" "libILUT.so.1"))
  (:windows "ILUT.dll")
  (t (:default "libILUT")))
(use-foreign-library ilut)

(defcenum renderer
  (:opengl 0)
  (:allegro 1)
  (:win32 2)
  (:direct3d8 3)
  (:direct3d9 4))

(defcenum state-definition
  (:palette-mode #x0600)
  (:opengl-conv #x0610)
  (:d3d-miplevels #x0620)
  (:maxtex-width #x0630)
  (:maxtex-height #x0631)
  (:maxtex-depth #x0632)
  (:gl-use-s3tc #x0634)
  (:d3d-use-dxtc #x0634)
  (:gl-gen-s3tc #x0635)
  (:d3d-gen-dxtc #x0635)
  (:s3tc-format #x0705)
  (:dxtc-format #x0705)
  (:d3d-pool #x0706)
  (:d3d-alpha-key-color #x0707)
  (:d3d-alpha-key-colour #x0707))

(define-foreign-type pathname-string-type ()
  ()
  (:actual-type :string)
  (:simple-parser pathname-string))
(eval-when (:compile-toplevel :load-toplevel)
  (defmethod expand-to-foreign-dyn (value var body (type pathname-string-type))
    `(with-foreign-string (,var (if (pathnamep ,value) (namestring ,value) ,value))
       ,@body)))

(defcfun ("ilutInit" init) :boolean)
(defcfun ("ilutRenderer" renderer) :boolean (renderer renderer))
(defcfun ("ilutEnable" enable) :boolean (state state-definition))
(defcfun ("ilutDisable" disable) :boolean (state state-definition))
(defcfun ("ilutGLLoadImage" gl-load-image) :uint (file-name pathname-string))

(defun init-devil ()
  (uid-il:init)
  #+nil(uid-ilut:init)) ; ILUT is broken on x86 for now, and I don't use anything that requires this

(defun shutdown-devil ()
  (uid-il:shutdown))

(defun reinit-devil ()
  (shutdown-devil)
  (init-devil))

(eval-when (:load-toplevel :execute)
  (reinit-devil))
