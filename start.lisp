(in-package :deal)

;;;;;;;;;; State
(defvar *server* (make-instance 'server))

(defun make-standard-deck (deck-name card-type extra-cards suits ranks)
  (make-instance 
   'deck
   :deck-name deck-name :card-type card-type
   :cards (append
	   extra-cards
	   (loop for s in suits 
	      append (loop for r in ranks 
			collect `((suit . ,s) (rank . ,r)))))))

;;;;; Default Decks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (decks *server*)
      (list (make-standard-deck 
	     "54-Card Standard" :french
	     (list "Joker" "Joker")
	     (list :hearts :clubs :spades :diamonds)
	     (list :ace 2 3 4 5 6 7 8 9 10 :jack :queen :king))
	    (make-standard-deck 
	     "Northern Italian" :n-italian nil
	     (list :denari :spade :coppe :bastoni)
	     (list :as 2 3 4 5 6 7 :fante :cavallo :re))
	    (make-standard-deck 
	     "Occult Tarot" :occult-tarot 
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