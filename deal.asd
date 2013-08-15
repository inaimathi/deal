;;;; deal.asd

(asdf:defsystem #:deal
  :serial t
  :description "Playtesting system for tabletop card games"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :depends-on (#:optima #:drakma #:hunchentoot #:cl-json #:cl-mop #:bordeaux-threads)
  :components ((:file "package")
	       (:file "util")
	       (:file "model/table")
	       (:file "model/server")
	       (:file "deal")
	       (:file "start")))