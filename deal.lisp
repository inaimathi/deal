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
(define-server-handler (game/new-private-table) ((passphrase :string))
  (insert! *server* (make-instance 'table :players (players *server*) :passphrase passphrase)))

(define-server-handler (game/new-public-table) ()
  (insert! *server* (make-instance 'table :players (players *server*))))

(define-table-handler (game/join-table) ()
  (insert! table *player*))

;; (define-handler (game/resume-table) ()
;;   :sitting-down-at-table)

(define-table-handler (play/move) ((thing-id :int) (x :int) (y :int) (z :int) (rot :int))
  ;; TODO
  (list :moving thing-id :to x y z rot))

(define-table-handler (play/take-control) ((thing-id :int))
  ;; TODO
  (list :assuming-direct-control-of thing-id))

(define-table-handler (play/flip) ((thing-id :int))
  ;; TODO
  (list :flipping thing-id))

(define-table-handler (play/new-stack) ((cards-or-stacks :json))
  ;; TODO
  (list :making-new-stack cards-or-stacks))

(define-table-handler (play/new-stack-from-deck) ((deck-name :string))
  (let ((stack (deck->stack *player* (assoc deck-name (decks *server*) :test #'string=))))
    (insert! table stack)
    (publish stack)))

;;;;; Stacks
(define-table-handler (stack/draw) ((stack-id :int) (num :int))
  (let ((stack (gethash stack-id (things table))))
    (with-slots (cards card-count) stack
      (loop repeat (min num card-count)
	 do (decf card-count)
	 do (push (pop cards) (hand *player*))))))

(define-table-handler (stack/peek-cards) ((stack-id :int) (min :int) (max :int))
  (let ((stack (gethash stack-id (things table))))
    (take (- max min) (drop (+ min 1) (cards stack)))))

(define-table-handler (stack/show) ((stack-id :int) (min :int) (max :int))
  (let ((stack (gethash stack-id (things table))))
    (take (- max min) (drop (+ min 1) (cards stack)))))

(define-table-handler (stack/reorder) ((stack-id :int) (min :int) (max :int))
  ;; TODO
  (list :reordering-cards min :to max :from stack-id))

(define-table-handler (stack/play) ((stack-id :int))
  (let ((stack (gethash stack-id (things table))))
    (check-type stack 'stack)
    (with-slots (card card-count) stack
      (insert! table (pop (cards stack))))))

(define-table-handler (stack/add-to) ((stack-id :int) (card-id :int))
  ;; TODO
  (list :adding card-id :to stack-id))

;;;;; Hand
(define-table-handler (hand/play) ((card-id :int) (face :facing))
  (let ((card (nth card-id (hand *player*))))
    (setf (face card) face)
    (remove-nth card-id (hand *player*))
    (insert! table card)
    (publish card)))

(define-table-handler (hand/play-to) ((card-id :int) (stack-id :int))
  ;; TODO
  (list :playing-card card-id :to stack-id))

(define-table-handler (hand/pick-up) ((card-id :int))
  (let ((card (gethash card-id (things table))))
    (check-type card 'card)
    (push card (hand *player*))
    (remhash card-id (things table))) (list :picking-up card-id))