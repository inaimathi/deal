;;;; deal.lisp
(in-package #:deal)

;;;;;;;;;; Handlers
;;;;; Getters
(define-handler (server-info) ()
  `((handlers . ,*handlers*)
    (public-tables . ,(hash-map (lambda (k v) 
				  `((id . ,k) (tag . ,(tag v)) (seated . ,(length (players v))) (of . ,(max-players v))))
				(public-tables *server*)))
    (decks . ,(mapcar #'car (decks *server*)))))

(define-handler (show-table) ((table :table))
  (redact table))

(define-handler (table-history) ((table :table))
  (history table))

(define-handler (my-hand) ()
   (hash-values (hand (session-value :player))))

;;;;; Lobby-related
(define-handler (lobby/speak) ((message :string))
  (ensure-player)
  (publish! *server* :said (take 255 message))
  :ok)

(define-handler (lobby/tag) ((new-tag :string))
  (let ((tg (take 255 new-tag))
	(old))
    (aif (session-value :player)
	 (setf old (tag it) 
	       (tag it) tg)
	 (setf it (make-instance 'player :tag tg)))
    (publish! *server* :changed-nick `((old-tag . ,old)))
    :ok))

(define-handler (lobby/new-private-table) ((passphrase :string))
  (with-lock-held ((lock *server*))
    (let ((player (make-instance 'player)))
      (setf (session-value :player) player)
      (insert! *server* (make-instance 'table :players (list player) :passphrase passphrase)))))

(define-handler (lobby/new-public-table) ()
  (with-lock-held ((lock *server*))
    (let ((player (make-instance 'player)))
      (setf (session-value :player) player)
      (insert! *server* (make-instance 'table :players (list player))))))

(define-handler (lobby/join-table) ((table :table) (passphrase :string))
  (with-table-lock
      (let ((player (make-instance 'player)))
	(setf (session-value :player) player)
	(insert! table player)
	(publish! table :joined)
	(redact table))))

;;;; Game related (once you're already at a table)
(define-handler (play/speak) ((table :table) (message :string))
  (publish! table :said `((message . ,(take 255 message))))
  :ok)

(define-handler (play/move) ((table :table) (thing :placeable) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (set-props thing x y z rot)
    (publish! table :moved  `((thing . ,(id thing)) (x . ,x) (y . ,y) (z . ,z) (rot . ,rot)))
    (redact table)))

(define-handler (play/take-control) ((table :table) (thing :placeable))
  (with-table-lock
    (setf (belongs-to thing) (id (session-value :player)))
    (publish! table :took-control `((thing . ,(id thing))))
    (redact table)))

(define-handler (play/flip) ((table :table) (thing :flippable))
  (with-table-lock
    (with-slots (face) thing
      (setf face (if (eq face :up) :down :up))
      (publish! table :flipped `((thing . ,(redact thing))))
      (redact table))))

(define-handler (play/new-stack-from-cards) ((table :table) (cards (:list card)))
  (with-table-lock
    (let* ((c (first cards))
	   (stack (make-instance 'stack :belongs-to (session-value :player) :x (x c) :y (y c) :z (z c) :rot (rot c))))
      (loop for card in cards 
	 do (remhash (id card) (things table))
	 do (insert! stack card))
      (publish! table :stacked-up `((stack . ,(redact stack)) (cards . ,(mapcar #'id cards))))
      (redact table))))

(define-handler (play/new-stack-from-deck) ((table :table) (deck-name :string) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (let ((stack (deck->stack (session-value :player) (cdr (assoc deck-name (decks *server*) :test #'string=)) :face face)))
      (set-props stack x y z rot)
      (insert! table stack)
      (publish! table :new-deck `((name . ,deck-name) (stack . ,(redact stack))))
      (redact table))))

;;;;; Stacks
(define-handler (stack/play) ((table :table) (stack :stack) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (let ((card (pop! stack)))
      (set-props card x y z rot)
      (insert! table card)
      (publish! table :played-from-stack `((stack . ,(id stack)) (card . ,(redact card))))
      (redact table))))

(define-handler (stack/add-to) ((table :table) (stack :stack) (card (:card :from-table)))
  (with-table-lock
    (move! card table stack)
    (publish! table :added-to-stack `((stack . ,(id stack)) (card . ,(id card))))
    (redact table)))

(define-handler (stack/merge) ((table :table) (stacks (:list stack)))
  (with-table-lock
    (let ((stack (first stacks)))
      (loop for s in (rest stacks)
	 do (dolist (c (cards s)) (insert! c (cards stack)))
	 do (remhash (id s) (things table)))
      (publish! table :merged-stacks `((stack . ,(redact stack)) (stacks ,@(mapcar #'id stacks))))
      (redact table))))

;;;;; Hand
(define-handler (hand/play) ((table :table) (card (:card :from-hand)) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (with-table-lock
    (setf (face card) face (x card) x (y card) y (z card) z (rot card) rot)
    (move! card (session-value :player) table)
    (publish! table :played-from-hand `((card . ,(redact card))))
    (redact table)))

(define-handler (hand/play-to) ((table :table) (card (:card :from-hand)) (stack :stack))
  (with-table-lock
    (move! card (session-value :player) stack)
    (publish! table :played-to-stack `((stack . ,(id stack)) (card . ,(redact card))))
    (redact table)))

(define-handler (hand/pick-up) ((table :table) (card (:card :from-table)))
  (with-table-lock
    (move! card table (session-value :player))
    (publish! table :picked-up `((card . ,(id card))))
    (redact table)))

;;;;; Non-table handlers
;;; These handlers don't respond with a redacted table object because it wouldn't make sense in context
(define-handler (play/roll) ((num-dice :int) (die-size :int) (modifier :int))
  (assert (and (> num-dice 0) (> die-size 0)))
  (let ((mod (cond ((> modifier 0) (format nil "+~a" modifier))
		   ((> 0 modifier) modifier)
		   (t nil))))
    (multiple-value-bind (total rolls) (roll num-dice die-size)
      (publish! table :rolled
		`((dice . ,(format nil "~ad~a~@[~a~]" num-dice die-size mod)) 
		  (total . ,(+ total modifier))
		  (rolls . ,rolls))))))

(define-handler (play/coin-toss) ()
  (publish! table :flipped-coin `((result . ,(pick (list :heads :tails))))))

(define-handler (stack/draw) ((table :table) (stack :stack) (num :int))
  (with-table-lock
    (loop with rep = (min num (card-count stack)) repeat rep
       do (insert! (session-value :player) (pop! stack)))
    (publish! table :drew-from `((stack . ,(id stack)) (count . ,num)))
    (hash-values (hand (session-value :player)))))

(define-handler (stack/peek-cards) ((table :table) (stack :stack) (min :int) (max :int))
  (publish! table :peeked `((stack . ,(id stack)) (count . ,(- max min))))
  (take (- max min) (drop (+ min 1) (cards stack))))

(define-handler (stack/show) ((table :table) (stack :stack) (min :int) (max :int))
  (publish! table :revealed `((stack . ,(id stack)) (cards ,@(take (- max min) (drop (+ min 1) (cards stack)))))))

;; (define-handler (stack/reorder) ((table :table) (stack :stack) (min :int) (max :int))
;;   ;; TODO
;;   (list :reordering-cards min :to max :from stack))