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
  ((text :accessor text :initarg :text)
   (image :accessor image :initform nil :initarg :image)))

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
  ((id :accessor id :initarg :id)
   (started :reader started :initform (get-universal-time))
   (players :accessor players :initform nil :initarg :players)
   (things :accessor things :initform nil :initarg :things)
   (passphrase :accessor passphrase :initform nil :initarg :passphrase)
   (tablecloth :accessor tablecloth :initform nil :initarg :tablecloth)
   (current-player :accessor current-player :initform nil :initarg :current-player)
   (events :accessor events :initform nil :initarg :events)
   (lock :initform (make-lock) :accessor lock)))

(defclass player ()
  ((id :accessor id :initarg :id)
   (hand :accessor hand :initform nil :initarg :hand)
   (events :accessor events :initform nil :initarg :events)))

(defclass server ()
  ((public-tables :accessor public-tables :initform (make-hash-table))
   (private-tables :accessor private-tables :initform (make-hash-table))
   (decks :accessor decks :initform nil :initarg :decks)
   (players :accessor players :initform (make-hash-table))
   (table-count :accessor table-count :initform 0)
   (max-tables :accessor max-tables :initform 10)
   (player-count :accessor player-count :initform 0)
   (max-players :accessor max-players :initform 50)
   (lock :initform (make-lock) :accessor lock)))

(defmethod insert! ((server server) (table table))
  (with-lock-held ((lock server))
    (let ((id (table-count server)))
      (setf (id table) id
            (gethash id (if (passphrase table) 
			    (private-tables server)
			    (public-tables server)))
	    table)
      (incf (table-count server))
      table)))

(defmethod insert! ((server server) (player player))
  (with-lock-held ((lock server))
    (let ((id (player-count server)))
      (setf (id player) id
	    (gethash id (players server)) player)
      (incf (player-count server))
      player)))

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

;;;;;;;;;; Handlers
;;;;; Getters
(define-handler (list-games) ()
  (hash-keys (public-tables *server*)))

(define-handler (list-decks) ()
  (mapcar #'car (decks *server*)))

;;;;; SSEs
;; game (also available to spectators)
;; yours

;;;;; Setters
(define-handler (game/new-private-table) ((passphrase :string))
  (insert! *server* (make-instance 'table :players (players *server*) :passphrase passphrase)))

(define-handler (game/new-public-table) ()
  (insert! *server* (make-instance 'table :players (players *server*))))

(define-table-handler (game/join-table) ()
  (push *player* (players table)) :joining-table)

;; (define-handler (game/resume-table) ()
;;   :sitting-down-at-table)

(define-table-handler (play/move) ((thing-id :int) (x :int) (y :int) (z :int) (rot :int))
  (list :moving thing-id :to x y z rot))

(define-handler (play/take-control) ((thing-id :int))
  (list :assuming-direct-control-of thing-id))

(define-handler (play/flip) ((thing-id :int))
  (list :flipping thing-id))

(define-handler (play/new-stack) ((cards :json))
  (list :adding-new-stack-from-cards cards))

(define-handler (play/new-stack-from-deck) ((deck-name :string))
  (push (deck->stack *player* (assoc deck-name (decks *server*) :test #'string=)) 
	(things (car (public-tables *server*))))
  (mapcar #'publish (things (car (public-tables *server*)))))

;;;;; Stacks
(define-handler (stack/draw) ((stack-id :int) (num :int))
  (list :drawing num :cards-from stack-id))

(define-handler (stack/peek-cards) ((stack-id :int) (min :int) (max :int))
  (list :peeking-at-cards min :to max :from stack-id))

(define-handler (stack/show) ((stack-id :int) (min :int) (max :int))
  (list :peeking-at-cards min :to max :from stack-id))

(define-handler (stack/reorder) ((stack-id :int) (min :int) (max :int))
  (list :reordering-cards min :to max :from stack-id))

(define-handler (stack/play) ((stack-id :int))
  (list :playing-top-card-from stack-id))

(define-handler (stack/add-to) ((stack-id :int) (card :int))
  (list :adding card :to stack-id))

;;;;; Hand
(define-handler (hand/play) ((card-id :int) (face :keyword))
  (assert (or (eq face :up) (eq face :down)))
  (list :playing-card card-id :to :board fc))

(define-handler (hand/play-to) ((card-id :int) (stack-id :int))
  (list :playing-card card-id :to stack-id))

(define-handler (hand/pick-up) ((card-id :int))
  (list :picking-up card-id))