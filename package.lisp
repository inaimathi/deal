;;;; package.lisp

(defpackage #:deal 
  (:use #:cl #:optima #:json #:cl-mop)
  (:import-from #:hunchentoot 
		#:start-session #:define-easy-handler))

