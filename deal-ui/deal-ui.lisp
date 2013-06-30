;;;; deal-ui.lisp

(in-package #:deal-ui)

;;;;;;;;;; JS util
(defparameter *debugging* t)

(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro fn (&body body) `(lambda () ,@body))

(defpsmacro log (&body body)
  (when *debugging*
    `(chain console (log (list ,@body)))))

;;;;;;;;;; deal-ui
(to-file "static/js/global.js"
	 (ps (log "Hello from the JS file!")
	     (doc-ready
	      (log "Hello from the document.ready event!"))))

(to-file "static/index.html"
	 (html (:head (:title "Tabletop Prototyping System - Deal")
		      (scripts "jquery.min.js" "jquery-ui.min.js" "global.js"))
	       (:body (:p "Testing..."))))
