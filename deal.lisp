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
(define-table-handler (event-source) ()
  (setf (header-out :cache-control) "no-cache"
	(content-type*) "text/event-stream")
  (events table))

;;;;; Setters
(define-server-handler (game/new-private-table) ((passphrase :string))
  (insert! *server* (make-instance 'table :players (players *server*) :passphrase passphrase)))

(define-server-handler (game/new-public-table) ()
  (insert! *server* (make-instance 'table :players (players *server*))))

(define-table-handler (game/join-table) ()
  (insert! table *player*))

;; (define-handler (game/resume-table) ()
;;   ;; TODO
;;   :sitting-down-at-table)

(define-table-handler (play/move) ((thing-id :int) (x :int) (y :int) (z :int) (rot :int))
  (let ((thing (gethash thing-id (things table))))
    (check-type thing placeable)
    (setf (x thing) x
	  (y thing) y
	  (z thing) z
	  (rot thing) rot)))

(define-table-handler (play/take-control) ((thing-id :int))
  (let ((thing (gethash thing-id (things table))))
    (check-type thing placeable)
    (setf (belongs-to thing) *player*)))

(define-table-handler (play/flip) ((thing-id :int))
  (let ((thing (gethash thing-id (things table))))
    (check-type thing flippable)
    (with-slots (face) thing
      (setf face (if (eq face :up) :down :up)))))

(define-table-handler (play/new-stack) ((cards-or-stacks :json))
  ;; TODO
  (list :making-new-stack cards-or-stacks))

(define-table-handler (play/new-stack-from-deck) ((deck-name :string))
  (let ((stack (deck->stack *player* (assoc deck-name (decks *server*) :test #'string=))))
    (insert! table stack)
    (publish stack)))

;;;;; Stacks
(define-table-handler (stack/draw) ((stack :stack) (num :int))
  (with-slots (cards card-count) stack
    (loop repeat (min num card-count)
       do (decf card-count)
       do (push (pop cards) (hand *player*)))))

(define-table-handler (stack/peek-cards) ((stack :stack) (min :int) (max :int))
  (take (- max min) (drop (+ min 1) (cards stack))))

(define-table-handler (stack/show) ((stack :stack) (min :int) (max :int))
  (take (- max min) (drop (+ min 1) (cards stack))))

;; (define-table-handler (stack/reorder) ((stack :stack) (min :int) (max :int))
;;   ;; TODO
;;   (list :reordering-cards min :to max :from stack))

(define-table-handler (stack/play) ((stack :stack))
  (with-slots (card card-count) stack
    (insert! table (pop (cards stack)))))

(define-table-handler (stack/add-to) ((stack :stack) (card-id :int))
  (let ((card (gethash card-id (things table))))
    (check-type card card)
    (insert! stack card)
    (delete! table card)
    (publish stack)))

;;;;; Hand
(define-table-handler (hand/play) ((card-id :int) (face :facing))
  (let ((card (nth card-id (hand *player*))))
    (check-type card card)
    (setf (face card) face)
    (remove-nth card-id (hand *player*))
    (insert! table card)
    (publish card)))

(define-table-handler (hand/play-to) ((card-id :int) (stack :stack))
  (let ((card (nth card-id (hand *player*))))
    (check-type card card)
    (insert! stack card)
    (remove-nth card-id (hand *player*))
    (publish stack)))

(define-table-handler (hand/pick-up) ((card-id :int))
  (let ((card (gethash card-id (things table))))
    (check-type card card)
    (push card (hand *player*))
    (remhash card-id (things table))) (list :picking-up card-id))