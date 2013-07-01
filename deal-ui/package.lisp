;;;; package.lisp

(defpackage #:deal-ui
  (:use #:cl #:cl-who #:parenscript #:cl-css)
  (:import-from :deal #:with-gensyms))

(in-package :deal-ui)

;; (setf *js-string-delimiter* #\")
(defparameter *debugging* t)