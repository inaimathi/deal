;;;; deal.lisp
(in-package #:deal)

;;;;;;;;;; Table and player
(defun deck->stack (player a-deck &key (face :down))
  (make-instance 'stack
		 :face face
		 :belongs-to player
		 :cards (shuffle (mapcar (lambda (str)
					   (make-instance 'card :text str :face face :belongs-to player))
					 a-deck))
		 :card-count (length a-deck)))

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

(define-table-handler (play/take-control) ((thing-id :int))
  (list :assuming-direct-control-of thing-id))

(define-table-handler (play/flip) ((thing-id :int))
  (list :flipping thing-id))

(define-table-handler (play/new-stack) ((cards-or-stacks :json))
  (list :making-new-stack cards-or-stacks))

(define-table-handler (play/new-stack-from-deck) ((deck-name :string))
  (push (deck->stack *player* (assoc deck-name (decks *server*) :test #'string=))
	(things table))
  (mapcar #'publish (things table)))

;;;;; Stacks
(define-table-handler (stack/draw) ((stack-id :int) (num :int))
  (list :drawing num :cards-from stack-id))

(define-table-handler (stack/peek-cards) ((stack-id :int) (min :int) (max :int))
  (list :peeking-at-cards min :to max :from stack-id))

(define-table-handler (stack/show) ((stack-id :int) (min :int) (max :int))
  (list :peeking-at-cards min :to max :from stack-id))

(define-table-handler (stack/reorder) ((stack-id :int) (min :int) (max :int))
  (list :reordering-cards min :to max :from stack-id))

(define-table-handler (stack/play) ((stack-id :int))
  (list :playing-top-card-from stack-id))

(define-table-handler (stack/add-to) ((stack-id :int) (card-id :int))
  (list :adding card-id :to stack-id))

;;;;; Hand
(define-table-handler (hand/play) ((card-id :int) (face :facing))
  (list :playing-card card-id :to :board face))

(define-table-handler (hand/play-to) ((card-id :int) (stack-id :int))
  (list :playing-card card-id :to stack-id))

(define-table-handler (hand/pick-up) ((card :int))
  (list :picking-up card-id))