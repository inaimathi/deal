(in-package :deal)

;;;;;;;;;; State
(defparameter *server* (make-instance 'server))

(defun make-deck (deck-name card-type extra-cards suits ranks &optional (template "~a of ~a"))
  (append (cons deck-name (cons card-type extra-cards))
	  (loop for s in suits append (loop for r in ranks collect (format nil template r s)))))

;;;;; Default Decks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (decks *server*)
      (list (make-deck "54-Card Standard" :french
		       (list "Joker" "Joker")
		       (list :hearts :clubs :spades :diamonds)
		       (list :ace 2 3 4 5 6 7 8 9 10 :jack :queen :king))
	    (make-deck "Northern Italian" :n-italian nil
		       (list :denari :spade :coppe :bastoni)
		       (list :as 2 3 4 5 6 7 :fante :cavallo :re)
		       "~a di ~a")
	    (make-deck "Occult Tarot" :occult-tarot 
		       (list "The Magician" "The High Priestess" "The Empress" 
			     "The Emperor" "The Hierophant" "The Lovers" "The Chariot" 
			     "Strength" "The Hermit" "Wheel of Fortune" "Justice" 
			     "The Hanged Man" "Death" "Temperance" "The Devil" 
			     "The Tower" "The Star" "The Moon" "The Sun" "Judgement" 
			     "The World" "The Fool")
		       (list :swords :wands :coins :cups)
		       (list :ace 2 3 4 5 6 7 8 9 10 :page :knight :queen :king))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *web-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *server-port*)))
(push (hunchentoot:create-folder-dispatcher-and-handler "/static/" "static/") hunchentoot:*dispatch-table*)