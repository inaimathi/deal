;;;; package.lisp

(defpackage #:deal 
  (:use #:cl #:optima #:json #:cl-mop #:bordeaux-threads)
  (:import-from #:hunchentoot #:start-session #:define-easy-handler))

