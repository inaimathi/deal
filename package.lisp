;;;; package.lisp

(defpackage #:deal 
  (:use #:cl #:optima #:json #:cl-mop #:bordeaux-threads)
  (:import-from #:hunchentoot #:start-session #:define-easy-handler))

(defgeneric insert! (container item)
  (:documentation "A generic insertion function. It takes a container object and an item, and inserts the second into the first in a destructive manner. It takes care of updating object state related to, but not part of, naive item insertion."))