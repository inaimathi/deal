(in-package :deal)

;;;;;;;;;; State
(defparameter *server* (make-instance 'server))

;;;;; TEST STATE
(with-slots (players player-count public-tables table-count decks) *server*
  (push (cons "54-Card Standard" 
	      (append (list "Joker" "Joker" "Rules for Poker") 
		      (loop for suit in (list :hearts :clubs :spades :diamonds) 
			 append (loop for rank from 1 to 13 
				   collect (format nil "~a of ~a" rank suit)))))
	decks)
  (insert! *server* (make-instance 'player))
  (insert! *server* (make-instance 'table :players (gethash 0 players))))

(defparameter *player* (gethash 0 (players *server*)))
;;;;;;;;;;;;;;;;;

;; (defvar *web-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *server-port*)))
;; (push (hunchentoot:create-folder-dispatcher-and-handler "/static/" "static/") hunchentoot:*dispatch-table*)