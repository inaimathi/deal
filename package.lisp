;;;; package.lisp
(defpackage #:deal 
  (:use #:cl #:optima #:json #:house)
  (:shadow #:publish!)
  (:import-from :cl-ppcre #:regex-replace-all)
  (:import-from :cl-fad #:list-directory)
  (:import-from :bordeaux-threads #:make-lock #:with-lock-held)
  (:import-from :alexandria #:with-gensyms)
  (:import-from :anaphora #:aif #:awhen #:it))

(in-package #:deal)
;;;;;;;;;; Config variables
(defparameter *server-port* 8080)
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; Generic definitions
(defgeneric insert! (container item)
  (:documentation "A generic insertion function. It takes a container object and an item, and inserts the second into the first in a destructive manner. It takes care of updating object state related to, but not part of, naive item insertion."))

(defgeneric delete! (container item)
  (:documentation "The inverse of `insert!`. Takes a container and an item, and removes the second from the first in a destructive matter. Undoes the same related object state that an insert! would have touched."))

(defgeneric redact (thing)
  (:documentation "Returns a copy of its argument with private information removed. Notably, doesn't show card text for face-down cards or stacks."))

(defgeneric serialize (thing)
  (:documentation "Returns a copy of its argument with private information intact, but table-specific data (such as `id` and `belongs-to`) removed. Used to save table states for later reloading."))

(defgeneric publish! (target player action-type &optional message)
  (:documentation "Publishes a new message to the stream of the designated target. The message will a JSON object constructed from `action-type`, some internally-generated metadata, and optionally `message`"))