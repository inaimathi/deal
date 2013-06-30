;;;; deal-ui.asd

(asdf:defsystem #:deal-ui
  :serial t
  :description "Describe deal-ui here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:deal #:parenscript #:cl-who #:cl-css)
  :components ((:file "deal-ui/package")
	       (:file "deal-ui/util")
               (:file "deal-ui/deal-ui")))

