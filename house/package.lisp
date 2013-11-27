(defpackage :house  
  (:use :cl #:optima #:cl-ppcre #:usocket)
  (:import-from #:alexandria :starts-with-subseq :with-gensyms)
  (:import-from #:anaphora :aif :awhen :aand :it)
  (:export :define-closing-handler :define-stream-handler :define-redirect-handler
	   :new-session! :get-session! :lookup
	   :subscribe! :publish!
	   :start
	   :->keyword))

(in-package :house)

(defparameter +max-request-size+ 50000)
(defparameter +max-request-age+ 30)