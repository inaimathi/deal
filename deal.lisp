;;;; deal.lisp
(in-package #:deal)

;;;;;;;;;; Handlers
;;;;; Getters
(define-handler (server-info) ()
  `((handlers . ,*handlers*)
    (public-tables . ,(hash-map 
		       (lambda (k v) 
			 `((id . ,k) (tag . ,(tag v)) (seated . ,(length (players v))) (of . ,(max-players v))))
		       (public-tables *server*)))
    (decks . ,(mapcar #'car (decks *server*)))))

(define-handler (look-table) ((table :table)) (redact table))

(define-handler (table-history) ((table :table)) (history table))

(define-handler (my-hand) ()
   (hash-values (hand (session-value :player))))

;;;;; Lobby-related
(define-handler (lobby/speak) ((message (:string :min 2 :max 255)))
  (ensure-player)
  (publish! *server* :said (escape-string message))
  :ok)

(define-handler (lobby/tag) ((new-tag (:string :max 255)))
  (let ((old))
    (aif (session-value :player)
	 (setf old (tag it) 
	       (tag it) new-tag)
	 (setf (session-value :player) (make-instance 'player :tag new-tag)))
    (publish! *server* :changed-nick `((old-tag . ,old)))
    :ok))

(define-handler (lobby/new-private-table) ((tag :string) (passphrase :string))
  (with-lock-held ((lock *server*))
    (let ((player (make-instance 'player))
	  (table (make-instance 'table :tag tag :passphrase passphrase)))
      (setf (session-value :player) player)
      (insert! *server* table)
      (insert! table player)
      (redact table))))

(define-handler (lobby/new-public-table) ((tag :string))
  (with-lock-held ((lock *server*))
    (let ((player (make-instance 'player))
	  (table (make-instance 'table :tag tag)))
      (setf (session-value :player) player)
      (insert! *server* table)
      (insert! table player)
      (with-slots (id tag players max-players) table
	(publish! *server* :started-table `((id . ,id) (tag . ,tag) (seated . ,(length players)) (of . ,max-players))))
      (redact table))))

(define-table-handler (lobby/join-table) ((table :table) (passphrase :string))
  (with-slots (id tag players max-players) table
    (let ((player (make-instance 'player))
	  (len (length players)))
      (assert (>= max-players len))
      (setf (session-value :player) player)
      (insert! table player)
      (publish! table :joined)
      (publish! *server*
		(if (>= (+ 1 len) max-players) :filled-table :joined)
		`((id . ,id)))
      (redact table))))

;;;; Game related (once you're already at a table)
(define-player-handler (play/speak) ((table :table) (message (:string :min 2 :max 255)))
  (publish! table :said `((message . ,(escape-string message))))
  :ok)

(define-player-handler (play/move) ((table :table) (thing :placeable) (x :int) (y :int) (z :int) (rot :int))
  (set-props thing x y z rot)
  (publish! table :moved  `((thing . ,(id thing)) (x . ,x) (y . ,y) (z . ,z) (rot . ,rot)))
  (redact table))

(define-player-handler (play/take-control) ((table :table) (thing :placeable))
  (setf (belongs-to thing) (id (session-value :player)))
  (publish! table :took-control `((thing . ,(id thing))))
  (redact table))

(define-player-handler (play/flip) ((table :table) (thing :flippable))
  (setf face (if (eq face :up) :down :up))
  (publish! table :flipped `((thing . ,(redact thing))))
  (redact table))

(define-player-handler (play/new-stack-from-cards) ((table :table) (cards (:list card)))
  (let* ((c ())first cards
	 (stack (make-instance 'stack :belongs-to (session-value :player) :x (x c) :y (y c) :z (z c) :rot (rot c))))
    (loop for card in cards 
       do (remhash (id card) (things table))
       do (insert! stack card))
    (publish! table :stacked-up `((stack . ,(redact stack)) (cards . ,(mapcar #'id cards))))
    (redact table)))

(define-player-handler (play/new-stack-from-deck) ((table :table) (deck-name :string) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (let ((stack (deck->stack (session-value :player) (cdr (assoc deck-name (decks *server*) :test #'string=)) :face face)))
    (set-props stack x y z rot)
    (insert! table stack)
    (publish! table :new-deck `((name . ,deck-name) (stack . ,(redact stack))))
    (redact table)))

;;;;; Stacks
(define-player-handler (stack/play) ((table :table) (stack :stack) (x :int) (y :int) (z :int) (rot :int))
  (let ((card (pop! stack)))
    (set-props card x y z rot)
    (insert! table card)
    (publish! table :played-from-stack `((stack . ,(id stack)) (card . ,(redact card))))
    (redact table)))

(define-player-handler (stack/add-to) ((table :table) (stack :stack) (card (:card :from-table)))
  (move! card table stack)
  (publish! table :added-to-stack `((stack . ,(id stack)) (card . ,(id card))))
  (redact table))

(define-player-handler (stack/merge) ((table :table) (stacks (:list stack)))
  (let ((stack (first stacks)))
    (loop for s in (rest stacks)
       do (dolist (c (cards s)) (insert! c (cards stack)))
       do (remhash (id s) (things table)))
    (publish! table :merged-stacks `((stack . ,(redact stack)) (stacks ,@(mapcar #'id stacks))))
    (redact table)))

;;;;; Hand
(define-player-handler (hand/play) ((table :table) (card (:card :from-hand)) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (setf (face card) face (x card) x (y card) y (z card) z (rot card) rot)
  (move! card (session-value :player) table)
  (publish! table :played-from-hand `((card . ,(redact card))))
  (redact table))

(define-player-handler (hand/play-to) ((table :table) (card (:card :from-hand)) (stack :stack))
  (move! card (session-value :player) stack)
  (publish! table :played-to-stack `((stack . ,(id stack)) (card . ,(redact card))))
  (redact table))

(define-player-handler (hand/pick-up) ((table :table) (card (:card :from-table)))
  (move! card table (session-value :player))
  (publish! table :picked-up `((card . ,(id card))))
  (redact table))

;;;;; Non-table handlers
;;; These handlers don't respond with a redacted table object because it wouldn't make sense in context
(define-player-handler (play/roll) ((table :table) (num-dice (:int :min 1)) (die-size (:int :min 2)) (modifier :int))
  (let ((mod (cond ((> modifier 0) (format nil "+~a" modifier))
		   ((> 0 modifier) modifier)
		   (t nil))))
    (multiple-value-bind (total rolls) (roll num-dice die-size)
      (publish! table :rolled
		`((dice . ,(format nil "~ad~a~@[~a~]" num-dice die-size mod)) 
		  (total . ,(+ total modifier))
		  (rolls . ,rolls))))))

(define-player-handler (play/coin-toss) ((table :table))
  (publish! table :flipped-coin `((result . ,(pick (list :heads :tails))))))

(define-player-handler (stack/draw) ((table :table) (stack :stack) (num :int))
  (loop with rep = (min num (card-count stack)) repeat rep
     do (insert! (session-value :player) (pop! stack)))
  (publish! table :drew-from `((stack . ,(id stack)) (count . ,num)))
  (hash-values (hand (session-value :player))))

(define-player-handler (stack/peek-cards) ((table :table) (stack :stack) (min :int) (max :int))
  (publish! table :peeked `((stack . ,(id stack)) (count . ,(- max min))))
  (take (- max min) (drop (+ min 1) (cards stack))))

(define-player-handler (stack/show) ((table :table) (stack :stack) (min :int) (max :int))
  (publish! table :revealed 
	    `((stack . ,(id stack)) (cards ,@(take (- max min) (drop (+ min 1) (cards stack)))))))

;; (define-handler (stack/reorder) ((table :table) (stack :stack) (min :int) (max :int))
;;   ;; TODO
;;   (list :reordering-cards min :to max :from stack))