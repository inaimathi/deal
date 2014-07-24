(in-package :deal)

;;;;;;;;;; State
(format t "Instantiating game server ...~%")
(defvar *server* (make-instance 'server))

(defun make-standard-deck (deck-name card-type extra-cards suits ranks)
  (make-instance 
   'deck
   :deck-name deck-name :card-type card-type
   :cards (append
	   (loop for c in extra-cards 
	      collect `((name . ,c)))
	   (loop for s in suits 
	      append (loop for r in ranks 
			collect `((suit . ,s) (rank . ,r)))))))

(format t "Making default decks ...~%")

;;;;; Default Minis/boads ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dir->uris (directory)
  (let ((uris))
    (cl-fad:walk-directory
     (merge-pathnames directory (asdf/system:system-source-directory :deal))
     (lambda (f) (push (path->uri f :stem-from "static") uris)))
    uris))

(defparameter *default-minis* (dir->uris "static/img/minis/"))
(defparameter *default-boards* (dir->uris "static/img/tablecloths/"))

;;;;; Default Decks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (decks *server*)
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
	       (list :ace 2 3 4 5 6 7 8 9 10 :page :knight :queen :king)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (format t "Starting HOUSE on ~a ...~%" *server-port*)
;; (start *server-port*)
