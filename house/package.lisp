(defpackage :house  
  (:use :cl #:optima #:cl-ppcre #:usocket)
  (:import-from #:alexandria :starts-with-subseq)
  (:import-from #:anaphora :aif :awhen :aand :it)
  (:export :define-closing-handler :define-stream-handler
	   :new-session! :get-session! :lookup
	   :subscribe! :publish!
	   :start))

(in-package :house)

(defparameter +max-request-size+ 50000)
(defparameter +max-request-age+ 30)