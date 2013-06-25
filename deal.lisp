;;;; deal.lisp

(in-package #:deal)

;;;;;;;;;; Game elements
(defclass placeable ()
  ((x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   (z :accessor z :initform 0 :initarg :z)
   (rot :accessor rot :initform 0 :initarg :rot)
   (belongs-to :accessor belongs-to :initarg :belongs-to)))

(defclass flippable (placeable)
  ((face :accessor face :initform :up :initarg :face)))

(defclass card (flippable)
  ((text :accessor text :initarg :text)))

(defclass stack (flippable)
  ((cards :accessor cards :initform nil :initarg :cards)
   (card-count :accessor card-count :initform 0 :initarg :card-count)
   (face :initform :down)))

(defclass counter (placeable)
  ((counter-value :accessor counter-value :initarg :counter-value)))

(defclass mini (placeable)
  ((sprite :accessor sprite :initarg :sprite)))

;;;;;;;;;; Table and player
(defclass table ()
  ((players :accessor players :initform nil :initarg :players)
   (things :accessor things :initform nil :initarg :things)
   (passphrase :accessor passphrase :initform nil :initarg :passphrase)
   (tablecloth :accessor tablecloth :initform nil :initarg :tablecloth)
   (current-player :accessor current-player :initform nil :initarg :current-player)
   (events :accessor events :initform nil :initarg :events)))

(defclass player ()
  ((id :accessor id :initarg :id)
   (hand :accessor hand :initform nil :initarg :hand)
   (events :accessor events :initform nil :initarg :events)))

(defun deck->stack (player a-deck &key (face :down))
  (make-instance 'stack
		 :face face
		 :belongs-to player
		 :cards (shuffle (mapcar (lambda (str)
					   (make-instance 'card :text str :face face :belongs-to player))
					 a-deck))
		 :card-count (length a-deck)))

(defmethod publish ((stack stack))
  (if-up stack stack
	 (cons '(cards) 
	       (remove-if (lambda (pair) (eq (first pair) 'cards)) 
			  (to-alist stack)))))

;;;;;;;;;; State
(defparameter *player* (make-instance 'player :id 0))
(defparameter *game* (make-instance 'table :players (list *player*)))

(defparameter *54-card-deck*
  (append (list "Joker" "Joker" "Rules for Poker") 
	  (loop for suit in (list :hearts :clubs :spades :diamonds) 
	     append (loop for rank from 1 to 13 
		       collect (format nil "~a of ~a" rank suit)))))

(defparameter *decks* (list (cons "54-Card Standard" *54-card-deck*)))
(defparameter *public-games* (list *game*))

;;;;;;;;;; Server State
;;;;;;;;;; Handlers

;;;;; Getters
(define-handler (list-games) ()
  *public-games*)

(define-handler (list-decks) ()
  (mapcar #'car *decks*))

;;;;; SSEs
;; game (also available to spectators)
;; yours

;;;;; Setters
;; (define-handler (game/join-table) ()
;;   :joining-table)

;; (define-handler (game/new-table) (passphrase)
;;   (list :laying-new-table passphrase))

;; (define-handler (game/resume-table) ()
;;   :sitting-down-at-table)

(define-handler (play/move) (thing-id x y z rot)
  (list :moving thing-id :to x y z rot))

(define-handler (play/take-control) (thing-id)
  (list :assuming-direct-control-of thing-id))

(define-handler (play/flip) (thing-id)
  (list :flipping thing-id))

(define-handler (play/new-stack) ((deck-name :string))
  (push (deck->stack *player* (assoc deck-name *decks* :test #'string=)) (things *game*))
  (mapcar #'publish (things *game*)))

;;;;; Stacks
(define-handler (stack/draw) (stack-id num)
  (list :drawing num :cards-from stack-id))

(define-handler (stack/peek-cards) (stack-id min max)
  (list :peeking-at-cards min :to max :from stack-id))

(define-handler (stack/show) (stack-id min max)
  (list :peeking-at-cards min :to max :from stack-id))

(define-handler (stack/reorder) (stack-id min max)
  (list :reordering-cards min :to max :from stack-id))

(define-handler (stack/play) (stack-id)
  (list :playing-top-card-from stack-id))

(define-handler (stack/add-to) (stack-id card)
  (list :adding card :to stack-id))

;;;;; Hand
(define-handler (hand/play) (card-id (face :keyword))
  (assert (or (eq fc :up) (eq fc :down)))
  (list :playing-card card-id :to :board fc))

(define-handler (hand/play-to) (card-id stack-id)
  (list :playing-card card-id :to stack-id))

(define-handler (hand/pick-up) (card-id)
  (list :picking-up card-id))