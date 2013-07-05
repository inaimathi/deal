;;;; deal.lisp
(in-package #:deal)

(define-handler (list-handlers) () *handlers*)

;;;;;;;;;; Handlers
;;;;; Getters
(define-handler (list-tables) ()
  (hash-keys (public-tables *server*)))

(define-handler (list-decks) ()
  (mapcar #'car (decks *server*)))

(define-handler (show-table) ((table :table))
  (redact table))

(define-handler (my-hand) ()
  (hand *player*))

;;;;; SSEs
(define-sse-handler (event-source) ((table :table))
  (events table))

;;;;; Table-related
(define-handler (game/new-private-table) ((passphrase :string))
  (with-lock-held ((lock *server*))
    (insert! *server* (make-instance 'table :players (players *server*) :passphrase passphrase))))

(define-handler (game/new-public-table) ()
  (with-lock-held ((lock *server*))
    (insert! *server* (make-instance 'table :players (players *server*)))))

(define-handler (game/join-table) ((table :table) (passphrase :string))
  (with-lock-held ((lock table))
    (insert! table *player*)
    (redact table)))

;;;; Game related (once you're already at a table)
(define-handler (play/move) ((table :table) (thing :placeable) (x :int) (y :int) (z :int) (rot :int))
  (with-lock-held ((lock table))
    (set-props thing x y z rot)
    (redact table)))

(define-handler (play/take-control) ((table :table) (thing :placeable))
  (with-lock-held ((lock table))
    (setf (belongs-to thing) (id *player*))
    (redact table)))

(define-handler (play/flip) ((table :table) (thing :flippable))
  (with-lock-held ((lock table))
    (with-slots (face) thing
      (setf face (if (eq face :up) :down :up))
      (redact table))))

;; (define-handler (play/new-stack) ((table :table) (cards-or-stacks :json))
;;   ;; TODO
;;   (with-lock-held ((lock table))
;;     (list :making-new-stack cards-or-stacks)))

(define-handler (play/new-stack-from-deck) ((table :table) (deck-name :string) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (with-lock-held ((lock table))
    (let ((stack (deck->stack *player* (assoc deck-name (decks *server*) :test #'string=) :face face)))
      (set-props stack x y z rot)
      (insert! table stack)
      (redact table))))

;;;;; Stacks
(define-handler (stack/play) ((table :table) (stack :stack) (x :int) (y :int) (z :int) (rot :int))
  (with-lock-held ((lock table))
    (with-slots (cards card-count) stack
      (let ((card (pop cards)))
	(set-props card x y z rot)
	(insert! table (pop cards))
	(redact table)))))

(define-handler (stack/add-to) ((table :table) (stack :stack) (card (:card :from-table)))
  (with-lock-held ((lock table))
    (insert! stack card)
    (delete! table card)
    (redact table)))

;;;;; Hand
(define-handler (hand/play) ((table :table) (card (:card :from-hand)) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (with-lock-held ((lock table))
    (setf (face card) face (x card) x (y card) y (z card) z (rot card) rot)
    (delete! *player* card)
    (insert! table card)
    (redact table)))

(define-handler (hand/play-to) ((table :table) (card (:card :from-hand)) (stack :stack))
  (with-lock-held ((lock table))
    (insert! stack card)
    (delete! *player* card)
    (redact table)))

(define-handler (hand/pick-up) ((table :table) (card (:card :from-table)))
  (with-lock-held ((lock table))
    (push card (hand *player*))
    (delete! table card)
    (redact table)))

;;;;; Odd ducks
;;; These handlers are the only ones that don't respond with a redacted table object
(define-handler (stack/draw) ((table :table) (stack :stack) (num :int))
  (with-lock-held ((lock table))
    (with-slots (cards card-count) stack
      (loop with rep = (min num card-count) repeat rep
	 do (let ((card (pop cards)))
	      (decf card-count)
	      (insert! *player* card)))
      (hand *player*))))

(define-handler (stack/peek-cards) ((table :table) (stack :stack) (min :int) (max :int))
  (take (- max min) (drop (+ min 1) (cards stack))))

(define-handler (stack/show) ((table :table) (stack :stack) (min :int) (max :int))
  ;; This one should publish the cards out to the table event stack
  (take (- max min) (drop (+ min 1) (cards stack))))

;; (define-handler (stack/reorder) ((table :table) (stack :stack) (min :int) (max :int))
;;   ;; TODO
;;   (list :reordering-cards min :to max :from stack))