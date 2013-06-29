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
(define-handler (event-source) ((table :table))
  (setf (header-out :cache-control) "no-cache"
	(content-type*) "text/event-stream")
  (events table))

;;;;; Setters
(define-handler (game/new-private-table) ((passphrase :string))
  (with-lock-held ((lock *server*))
    (insert! *server* (make-instance 'table :players (players *server*) :passphrase passphrase))))

(define-handler (game/new-public-table) ()
  (with-lock-held ((lock *server*))
    (insert! *server* (make-instance 'table :players (players *server*)))))

(define-handler (game/join-table) ((table :table))
  (with-lock-held ((lock table))
    (insert! table *player*)))

;; (define-handler (game/resume-table) ()
;;   ;; TODO
;;   :sitting-down-at-table)

(define-handler (play/move) ((table :table) (thing :placeable) (x :int) (y :int) (z :int) (rot :int))
  (with-lock-held ((lock table))
    (setf (x thing) x
	  (y thing) y
	  (z thing) z
	  (rot thing) rot)))

(define-handler (play/take-control) ((table :table) (thing :placeable))
  (with-lock-held ((lock table))
    (setf (belongs-to thing) *player*)))

(define-handler (play/flip) ((table :table) (thing :flippable))
  (with-lock-held ((lock table))
    (with-slots (face) thing
      (setf face (if (eq face :up) :down :up)))))

;; (define-handler (play/new-stack) ((table :table) (cards-or-stacks :json))
;;   ;; TODO
;;   (with-lock-held ((lock table))
;;     (list :making-new-stack cards-or-stacks)))

(define-handler (play/new-stack-from-deck) ((table :table) (deck-name :string))
  (with-lock-held ((lock table))
    (let ((stack (deck->stack *player* (assoc deck-name (decks *server*) :test #'string=))))
      (insert! table stack)
      (publish stack))))

;;;;; Stacks
(define-handler (stack/draw) ((table :table) (stack :stack) (num :int))
  (with-lock-held ((lock table))
    (with-slots (cards card-count) stack
      (loop repeat (min num card-count)
	 do (decf card-count)
	 do (push (pop cards) (hand *player*))))))

(define-handler (stack/peek-cards) ((table :table) (stack :stack) (min :int) (max :int))
  (take (- max min) (drop (+ min 1) (cards stack))))

(define-handler (stack/show) ((table :table) (stack :stack) (min :int) (max :int))
  (take (- max min) (drop (+ min 1) (cards stack))))

;; (define-handler (stack/reorder) ((table :table) (stack :stack) (min :int) (max :int))
;;   ;; TODO
;;   (list :reordering-cards min :to max :from stack))

(define-handler (stack/play) ((table :table) (stack :stack))
  (with-lock-held ((lock table))
    (with-slots (card card-count) stack
      (insert! table (pop (cards stack))))))

(define-handler (stack/add-to) ((table :table) (stack :stack) (card (:card :from-table)))
  (with-lock-held ((lock table))
    (insert! stack card)
    (delete! table card)
    (publish stack)))

;;;;; Hand
(define-handler (hand/play) ((table :table) (card-id :int) (face :facing))
  (with-lock-held ((lock table))
    (let ((card (nth card-id (hand *player*))))
      (check-type card card)
      (setf (face card) face)
      (remove-nth card-id (hand *player*))
      (insert! table card)
      (publish card))))

(define-handler (hand/play-to) ((table :table) (card-id :int) (stack :stack))
  (with-lock-held ((lock table))
    (let ((card (nth card-id (hand *player*))))
      (check-type card card)
      (insert! stack card)
      (remove-nth card-id (hand *player*))
      (publish stack))))

(define-handler (hand/pick-up) ((table :table) (card (:card :from-table)))
  (with-lock-held ((lock table))
    (push card (hand *player*))
    (remhash (id card) (things table))
    (publish table)))