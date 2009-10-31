;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; input.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; SDL key/mouse constant translation
;;;
(defparameter *all-keys*
  '((:sdl-key-up :up) (:sdl-key-down :down) (:sdl-key-left :left) (:sdl-key-right :right)
    (:sdl-key-a :a) (:sdl-key-b :b) (:sdl-key-c :c) (:sdl-key-d :d) (:sdl-key-e :e) (:sdl-key-f :f)
    (:sdl-key-g :g) (:sdl-key-h :h) (:sdl-key-i :i) (:sdl-key-j :j) (:sdl-key-k :k) (:sdl-key-l :l)
    (:sdl-key-m :m) (:sdl-key-n :n) (:sdl-key-o :o) (:sdl-key-p :p) (:sdl-key-q :q) (:sdl-key-r :r)
    (:sdl-key-s :s) (:sdl-key-t :t) (:sdl-key-u :u) (:sdl-key-v :v) (:sdl-key-w :w) (:sdl-key-x :x)
    (:sdl-key-y :y) (:sdl-key-z :z) (:sdl-key-1 :1) (:sdl-key-2 :2) (:sdl-key-3 :3) (:sdl-key-4 :4)
    (:sdl-key-5 :5) (:sdl-key-6 :6) (:sdl-key-7 :7) (:sdl-key-8 :8) (:sdl-key-9 :9) (:sdl-key-0 :0)
    (:sdl-key-space :space) (:sdl-key-quote :quote) (:sdl-key-comma :comma) (:sdl-key-menu :menu)
    (:sdl-key-tab :tab) (:sdl-key-semicolon :semicolon) (:sdl-key-delete :delete)
    (:sdl-key-insert :insert) (:sdl-key-period :period) (:sdl-key-lctrl :left-ctrl)
    (:sdl-key-rctrl :right-ctrl) (:sdl-key-lalt :left-alt) (:sdl-key-ralt :right-alt)
    (:sdl-key-lsuper :left-super) (:sdl-key-rsuper :right-super) (:sdl-key-compose :compose)
    (:sdl-key-escape :escape) (:sdl-key-return :return) (:sdl-key-backspace :backspace)
    (:sdl-key-backquote :backquote) (:sdl-key-rightbracket :right-bracket)
    (:sdl-key-leftbracket :left-bracket) (:sdl-key-backslash :backslash) (:sdl-key-slash :slash)
    (:sdl-key-rshift :right-shift) (:sdl-key-lshift :left-shift) (:sdl-key-equals :equals)
    (:sdl-key-minus :minus) (:sdl-key-plus :plus) (:sdl-key-print :print)
    (:sdl-key-scrollock :scroll-lock) (:sdl-key-numlock :num-lock) (:sdl-key-capslock :caps-lock)
    (:sdl-key-pageup :page-up) (:sdl-key-pagedown :page-down) (:sdl-key-end :end) (:sdl-key-home :home)
    (:sdl-key-pause :pause) (:sdl-key-kp-plus :keypad-plus) (:sdl-key-kp-minus :keypad-minus)
    (:sdl-key-kp-multiply :keypad-multiply) (:sdl-key-kp-divide :keypad-divide)
    (:sdl-key-kp-period :keypad-period) (:sdl-key-kp-enter :keypad-enter) (:sdl-key-kp9 :keypad-9)
    (:sdl-key-kp8 :keypad-8) (:sdl-key-kp7 :keypad-7) (:sdl-key-kp4 :keypad-4) (:sdl-key-kp5 :keypad-5)
    (:sdl-key-kp6 :keypad-6) (:sdl-key-kp1 :keypad-1) (:sdl-key-kp2 :keypad-2) (:sdl-key-kp3 :keypad-3)
    (:sdl-key-kp0 :keypad-0) (:sdl-key-f1 :f1) (:sdl-key-f2 :f2) (:sdl-key-f3 :f3) (:sdl-key-f4 :f4)
    (:sdl-key-f5 :f5) (:sdl-key-f6 :f6) (:sdl-key-f7 :f7) (:sdl-key-f8 :f8) (:sdl-key-f9 :f9)
    (:sdl-key-f10 :f10) (:sdl-key-f11 :f11) (:sdl-key-f12 :f12) (:sdl-key-mod-lshift :left-shift)
    (:sdl-key-mod-rshift :right-shift) (:sdl-key-mod-rctrl :right-ctrl) (:sdl-key-mod-lctrl :left-ctrl)
    (:sdl-key-mod-lalt :left-alt) (:sdl-key-mod-ralt :right-alt) (:sdl-key-mod-num :num-lock)))

(defparameter *key-table*
  (let ((table (make-hash-table :test #'eq)))
    (loop for (sdl uid) in *all-keys*
         do (setf (gethash sdl table) uid))
    table))

(defun translate-key (key)
  (let ((translation (gethash key *key-table*)))
    (or translation key)))

(defun translate-key-list (keylist)
  (mapcar #'translate-key
          keylist))