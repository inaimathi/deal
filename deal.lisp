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
  (hash-values (hand (session-value :player))))

;;;;; SSEs
(define-sse-handler (event-source) ((table :table))
  (events table))

;;;;; Table-related
(define-handler (game/new-private-table) ((passphrase :string))
  (with-lock-held ((lock *server*))
    (setf (session-value :player) (make-instance 'player))
    (insert! *server* (make-instance 'table :players (list (session-value :player)) :passphrase passphrase))))

(define-handler (game/new-public-table) ()
  (with-lock-held ((lock *server*))
    (setf (session-value :player) (make-instance 'player))
    (insert! *server* (make-instance 'table :players (list (session-value :player))))))

(define-handler (game/join-table) ((table :table) (passphrase :string))
  (with-table-lock
    (setf (session-value :player) (make-instance 'player))
    (insert! table (session-value :player))
    (redact table)))

;;;; Game related (once you're already at a table)
(define-handler (play/move) ((table :table) (thing :placeable) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (set-props thing x y z rot)
    (redact table)))

(define-handler (play/take-control) ((table :table) (thing :placeable))
  (with-table-lock
    (setf (belongs-to thing) (id (session-value :player)))
    (redact table)))

(define-handler (play/flip) ((table :table) (thing :flippable))
  (with-table-lock
    (with-slots (face) thing
      (setf face (if (eq face :up) :down :up))
      (redact table))))

(define-handler (play/new-stack-from-cards) ((table :table) (cards (:list card)))
  (with-table-lock
    (let* ((c (first cards))
	   (stack (make-instance 'stack :belongs-to (session-value :player) :x (x c) :y (y c) :z (z c) :rot (rot c))))
      (loop for card in cards 
	 do (remhash (id card) (things table))
	 do (insert! stack card))
      (redact table))))

(define-handler (play/new-stack-from-deck) ((table :table) (deck-name :string) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (let ((stack (deck->stack (session-value :player) (cdr (assoc deck-name (decks *server*) :test #'string=)) :face face)))
      (set-props stack x y z rot)
      (insert! table stack)
      (redact table))))



;;;;; Stacks
(define-handler (stack/play) ((table :table) (stack :stack) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (let ((card (pop! stack)))
      (set-props card x y z rot)
      (insert! table card)
      (redact table))))

(define-handler (stack/add-to) ((table :table) (stack :stack) (card (:card :from-table)))
  (with-table-lock
    (move! card table stack)
    (redact table)))

(define-handler (stack/merge) ((table :table) (stacks (:list stack)))
  (with-table-lock
    (let ((stack (first stacks)))
      (loop for s in (rest stacks)
	 do (dolist (c (cards s)) (insert! c (cards stack)))
	 do (remhash (id s) (things table)))
      (redact table))))

;;;;; Hand
(define-handler (hand/play) ((table :table) (card (:card :from-hand)) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (setf (face card) face (x card) x (y card) y (z card) z (rot card) rot)
    (move! card (session-value :player) table)
    (redact table)))

(define-handler (hand/play-to) ((table :table) (card (:card :from-hand)) (stack :stack))
  (with-table-lock
    (move! card (session-value :player) stack)
    (redact table)))

(define-handler (hand/pick-up) ((table :table) (card (:card :from-table)))
  (with-table-lock
    (move! card table (session-value :player))
    (redact table)))

;;;;; Non-table handlers
;;; These handlers don't respond with a redacted table object because it wouldn't make sense in context
(define-handler (play/roll) ((num-dice :int) (die-size :int) (modifier :int))
  (assert (and (> num-dice 0) (> die-size 0)))
  (let ((mod (cond ((> modifier 0) (format nil "+~a" modifier))
		   ((> 0 modifier) modifier)
		   (t nil))))
    (multiple-value-bind (total rolls) (roll num-dice die-size)
      `((dice . ,(format nil "~ad~a~@[~a~]" num-dice die-size mod)) (total . ,(+ total modifier)) (rolls . ,rolls)))))

(define-handler (play/coin-toss) ()
  (pick (list :heads :tails)))

(define-handler (stack/draw) ((table :table) (stack :stack) (num :int))
  (with-table-lock
    (loop with rep = (min num (card-count stack)) repeat rep
       do (insert! (session-value :player) (pop! stack)))
    (hash-values (hand (session-value :player)))))

(define-handler (stack/peek-cards) ((table :table) (stack :stack) (min :int) (max :int))
  (take (- max min) (drop (+ min 1) (cards stack))))

(define-handler (stack/show) ((table :table) (stack :stack) (min :int) (max :int))
  ;;   This one should publish the cards out to the table event stack, 
  ;; rather than just notifying everyone of the action.
  (take (- max min) (drop (+ min 1) (cards stack))))

;; (define-handler (stack/reorder) ((table :table) (stack :stack) (min :int) (max :int))
;;   ;; TODO
;;   (list :reordering-cards min :to max :from stack))