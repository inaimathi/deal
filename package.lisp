;;;; package.lisp

(defpackage #:deal 
  (:use #:cl #:optima #:json #:cl-mop #:bordeaux-threads)
  (:import-from #:hunchentoot #:start-session #:define-easy-handler))

(defgeneric insert! (container item)
  (:documentation "A generic insertion function. It takes a container object and an item, and inserts the second into the first in a destructive manner. It takes care of updating object state related to, but not part of, naive item insertion."))

(defgeneric delete! (container item)
  (:documentation "The inverse of `insert!`. Takes a container and an item, and removes the second from the first in a destructive matter. Undoes the same related object state that an insert! would have touched."))

(defgeneric publish (thing)
  (:documentation "Passes the given thing out to the SSE handler.
A better name for this might be `redact`, because it often doesn't publish full info. For instance, when a card or stack is flipped, it won't publish text or image."))