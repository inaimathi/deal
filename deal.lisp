;;;; deal.lisp
(in-package #:deal)

(define-handler (root) ()
  (redirect "/static/index.html"))

;;;;;;;;;; Player-info related
(define-handler (server-info) ()
  (hash :handlers *handlers*
	:public-tables (loop for tbl being the hash-values of (public-tables *server*)
			  collect (obj->hash tbl () id tag player-count max-players))
	:decks (mapcar #'deck-name (decks *server*))
	:minis *mini-uris*
	:tablecloths *tablecloth-uris*))

(define-handler (get-session) ()
  (if (session-value :player)
      (with-slots (id tag current-table hand) (session-value :player)
	(hash :id id :tag tag :current-table (aif current-table (redact it)) :hand hand))
      (let ((player (make-instance 'player :tag "Anonymous Coward")))
	(setf (session-value :player) player)
	(with-slots (id tag) player
	  (hash :id id :tag tag :current-table nil :hand nil)))))

(define-handler (look/table) ((table :table)) 
  (redact table))

(define-handler (look/hand) () 
  (hash-values (hand (session-value :player))))

(define-handler (rename) ((new-tag (:string :max 255)))
  (assert (session-value :player))
  (let* ((player (session-value :player))
	 (old (tag player)))
    (when (string/= new-tag (tag player))
      (setf (tag player) new-tag)
      (publish! (aif (current-table player) it *server*) :changed-tag `((old-tag . ,old))))
    :ok))

;;;;;;;;;; Lobby actions
(define-handler (lobby/speak) ((message (:string :min 2 :max 255)))
  (assert (session-value :player))
  (publish! *server* :said `((message . ,(escape-string message))))
  :ok)

(define-handler (lobby/new-table) ((tag :string) (passphrase :string))
  (assert (session-value :player))
  (with-lock-held ((lock *server*))
    (let ((player (session-value :player))
	  (table (make-instance 'table :tag tag)))
      (insert! *server* table)
      (insert! table player)
      (unless (string= passphrase "")
	(setf (passphrase table) passphrase)
	(with-slots (id tag player-count max-players) table
	  (publish! *server* :started-table `((table ((id . ,id) (tag . ,tag) (seated . ,player-count) (of . ,max-players)))))))
      (redact table))))

(define-table-handler (lobby/join-table) ((table :table) (passphrase :string))
  (assert (and (session-value :player) (not (full? table))))
  (with-slots (id players) table
    (let ((player (session-value :player)))
      (unless (member player (players table))
	(insert! table player)
	(publish! table :joined `((table . ,(id table))))
	(publish! *server*
		  (if (full? table) :filled-table :joined)
		  `((id . ,id))))
      (redact table))))

;;;;;;;;;; Table related
;;;;; Direct table actions
(define-player-handler (table/save) ((table :table))
  (assert (= (player-count table) 1) nil "You can't save a table once the game has started.")
  (setf (header-out :content-type) "application/json"
	(header-out :content-disposition) "attachment; filename=\"game.json\"")
  (serialize table))

(define-player-handler (table/load) ((table :table) (file :json-file))
  (assert (= (player-count table) 1) nil "You can't load a table once the game has started.")
  (loop with player = (session-value :player)
     for thing in (getj :things file)
     do (case (intern (string-upcase (getj :type thing)) :keyword)
	  (:stack 
	   (insert! table (stack<-json player thing)))
	  (:card
	   (insert! table (card<-json player thing)))))
  (publish! table :loaded `((things . ,(length (getj :things file)))))
  :ok)

(define-player-handler (table/leave) ((table :table))
  (let ((player (session-value :player)))
    (delete! table player)
    (setf (current-table player) nil)
    (unless (passphrase table)
      (publish! table :left)
      (publish! *server* :left 
		`((table ((id . ,(id table)) (tag . ,(tag table)) (seated . ,(player-count table)) (of . ,(max-players table)))))))
    :ok))

(define-player-handler (table/tablecloth) ((table :table) (tablecloth-uri :string :max 255))
  (setf (tablecloth table) tablecloth-uri)
  (publish! table :tablecloth `((tablecloth . ,tablecloth-uri)))
  :ok)

(define-player-handler (table/speak) ((table :table) (message (:string :min 2 :max 255)))
  (publish! table :said `((message . ,(escape-string message))))
  :ok)

(define-player-handler (table/roll) ((table :table) (num-dice (:int :min 1 :max 128)) (die-size (:int :min 2 :max 4096)) (modifier (:int :min -4096 :max 4096)))
  (let ((mod (cond ((> modifier 0) (format nil "+~a" modifier))
		   ((> 0 modifier) modifier)
		   (t nil))))
    (multiple-value-bind (total rolls) (roll num-dice die-size)
      (publish! table :rolled
		`((dice . ,(format nil "~ad~a~@[~a~]" num-dice die-size mod)) 
		  (total . ,(+ total modifier))
		  (rolls . ,rolls)))))
  :ok)

(define-player-handler (table/coin-toss) ((table :table))
  (publish! table :flipped-coin `((result . ,(pick (list :heads :tails)))))
  :ok)

(define-player-handler (table/ping) ((table :table) (x :int) (y :int) (z :int))
  (publish! table :pinged `((x . ,x) (y . ,y) (z . ,z))))

;;;;; Table element creation handlers
(define-player-handler (table/new/mini) ((table :table) (image-uri :string :max 255) (x :int) (y :int) (z :int) (rot :int))
  (let ((mini (make-instance 'mini :belongs-to (id (session-value :player)))))
    (set-props mini image-uri x y z rot)
    (insert! table mini)
    (publish! table :placed-mini `((mini . ,(redact mini))))
    :ok))

(define-player-handler (table/new/note) ((table :table) (text :string :max 255) (x :int) (y :int) (z :int) (rot :int))
  (let ((note (make-instance 'note :belongs-to (id (session-value :player)))))
    (set-props note x y z rot text)
    (insert! table note)
    (publish! table :placed-note `((note . ,(redact note))))
    :ok))

(define-player-handler (table/new/note-on) ((table :table) (text :string :max 255) (thing :placeable))
  (let ((note (make-instance 'note :belongs-to (id (session-value :player)) 
			     :attached-to (id thing) :text text)))
    (insert! table note)
    (publish! table :placed-note `((note . ,(redact note))))
    :ok))

(define-player-handler (table/new/stack-from-cards) ((table :table) (cards (:list :card)))
  (let* ((c (first cards))
	 (stack (make-instance 'stack :belongs-to (id (session-value :player)) :x (x c) :y (y c) :z (z c) :rot (rot c))))
    (loop for card in cards 
       do (delete! table card) do (insert! stack card))
    (publish! table :stacked-up `((stack . ,(redact stack)) (cards . ,(mapcar #'id cards))))
    :ok))

(define-player-handler (table/new/stack-from-deck) ((table :table) (deck-name :string) (x :int) (y :int) (z :int) (rot :int))
  (let ((stack (stack<-deck (session-value :player) (find-deck deck-name))))
    (set-props stack x y z rot)
    (insert! table stack)
    (publish! table :new-deck `((name . ,deck-name) (stack . ,(redact stack))))
    :ok))

(define-player-handler (table/new/stack-from-json) ((table :table) (deck :json) (x :int) (y :int) (z :int) (rot :int))
  (let ((stack (stack<-json (session-value :player) deck)))
    (assert (cards stack))
    (set-props stack x y z rot)
    (insert! table stack)
    (publish! table :new-deck `((name . ,(getj :deck-name deck)) (stack . ,(redact stack))))
    :ok))

(define-player-handler (table/play) ((table :table) (card (:card :from-hand)) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (set-props card face x y z rot)
  (move! card (session-value :player) table)
  (publish! table :played-from-hand `((card . ,(redact card))))
  :ok)

;;;;; General element actions
(define-player-handler (table/remove) ((table :table) (thing :placeable))
  (delete! table thing)
  (publish! table :removed `((thing . ,(id thing))))
  :ok)

(define-player-handler (table/move) ((table :table) (thing :placeable) (x :int) (y :int) (z :int) (rot :int))
  (set-props thing x y z rot)
  (publish! table :moved  `((thing . ,(id thing)) (x . ,x) (y . ,y) (z . ,z) (rot . ,rot)))
  :ok)

(define-player-handler (table/take) ((table :table) (thing :placeable))
  (setf (belongs-to thing) (id (session-value :player)))
  (publish! table :took-control `((thing . ,(id thing))))
  :ok)

(define-player-handler (table/attach) ((table :table) (child :placeable) (parent :placeable))
  (pushnew (id parent) (attached-to child))
  (publish! table :attached `((child . ,(id child)) (parent . ,(id parent))))
  :ok)

(define-player-handler (table/detach-from) ((table :table) (child :placeable) (parent :placeable))
  (setf (attached-to child) (remove (id parent) (attached-to child)))
  (publish! table :detached-from `((child . ,(id child)) (parent . ,(id parent))))
  :ok)

(define-player-handler (table/detach) ((table :table) (child :placeable))
  (setf (attached-to child) nil)
  (publish! table :detached `((child . ,(id child))))
  :ok)

;;;;; Stack-specific actions
(define-player-handler (table/stack/play-top-card) ((table :table) (stack :stack) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (let ((card (pop! stack)))
    (set-props card face x y z rot)
    (insert! table card)
    (publish! table :played-from-stack `((stack . ,(id stack)) (card . ,(redact card))))
    :ok))

(define-player-handler (table/stack/play) ((table :table) (stack :stack) (card-id :keyword) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (let ((card (find card-id (cards stack) :key #'id)))
    (assert card)
    (set-props card face x y z rot)
    (publish! table :played-from-stack `((stack . ,(id stack)) (card . ,(redact card))))
    (move! card stack table)
    :ok))

(define-player-handler (table/stack/add-to) ((table :table) (stack :stack) (card (:card :from-table)))
  (publish! table :added-to-stack `((stack . ,(id stack)) (card . ,(id card))))
  (move! card table stack)
  :ok)

(define-player-handler (table/stack/merge) ((table :table) (stack :stack) (stack-two :stack))
  (dolist (c (cards stack-two)) (insert! stack c))
  (delete! table stack-two)
  (publish! table :merged-stacks `((stack . ,(redact stack)) (merged ,(id stack-two))))
  :ok)

(define-player-handler (table/stack/shuffle) ((table :table) (stack :stack))
  (setf (cards stack) (shuffle (cards stack)))
  (publish! table :shuffled `((stack . ,(id stack))))
  :ok)

(define-player-handler (table/stack/play-to) ((table :table) (card (:card :from-hand)) (stack :stack))
  (setf (face card) :down)
  (move! card (session-value :player) stack)
  (publish! table :played-to-stack `((stack . ,(id stack)) (card . ,(redact card))))
  :ok)

(define-player-handler (table/stack/draw) ((table :table) (stack :stack) (num :int))
  (loop with rep = (min num (card-count stack)) repeat rep
     do (let ((card (pop! stack)))
	  (insert! (session-value :player) card)))
  (when (zerop (card-count stack))
    (delete! table stack))
  (publish! table :drew-from `((stack . ,(id stack)) (count . ,num)))
  (hash-values (hand (session-value :player))))

(define-player-handler (table/stack/peek) ((table :table) (stack :stack) (min (:int :min 0)) (max :int))
  (publish! table :peeked `((stack . ,(id stack)) (count . ,(- max min))))
  (take (- max min) (drop min (cards stack))))

(define-player-handler (table/stack/take) ((table :table) (stack :stack) (card-id :keyword))
  (let ((card (find card-id (cards stack) :key #'id)))
    (assert card)
    (move! card stack (session-value :player))
    (publish! table :took-from `((stack . ,(id stack))))
    card))

(define-handler (table/stack/reorder) ((table :table) (stack :stack) (card-order (:list :keyword)))
  (with-slots (cards) stack
    (let* ((count (length card-order))
	   (head (take count cards))
	   (changed (loop for c-id in card-order
		       for card = (find c-id head :key #'id)
		       do (assert card) collect card)))
      (setf cards (append changed (drop count cards)))))
  (publish! table :reordered `((stack . ,(id stack)) (count . ,(length card-order))))
  :ok)

;;;;; Card-specific actions
(define-player-handler (table/card/flip) ((table :table) (card (:card :from-table)))
  (setf (face card) (if (eq (face card) :up) :down :up))
  (publish! table :flipped `((card . ,(redact card))))
  :ok)

(define-player-handler (table/card/pick-up) ((table :table) (card (:card :from-table)))
  (move! card table (session-value :player))
  (publish! table :picked-up `((card . ,(id card))))
  (hash-values (hand (session-value :player))))