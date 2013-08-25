(in-package :deal)

;;;;;;;;;; State
(defparameter *server* (make-instance 'server))

(defun make-deck (deck-name card-type extra-cards suits ranks &optional (template "~a of ~a"))
  (append (cons deck-name (cons card-type extra-cards))
	  (loop for s in suits append (loop for r in ranks collect (format nil template r s)))))

;;;;; Default Decks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (decks *server*)
      (list (make-deck "54-Card Standard" :french
		       (list "Joker" "Joker" "Rules for Draw and Stud Poker")
		       (list :hearts :clubs :spades :diamonds)
		       (trivial-range 1 13))
	    (make-deck "Northern Italian" :n-italian nil
		       (list :denari :spade :coppe :bastoni)
		       (list 1 2 3 4 5 6 7 :fante :cavallo :re)
		       "~a di ~a")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *web-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *server-port*)))
(push (hunchentoot:create-folder-dispatcher-and-handler "/static/" "static/") hunchentoot:*dispatch-table*)