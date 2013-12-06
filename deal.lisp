;;;; deal.lisp
(in-package #:deal)

(new-session-hook!
  (setf (lookup :player session) (make-instance 'player :tag "Anonymous Coward")))

(define-redirect-handler (root :permanent? t)
    "/static/index.html")

(define-stream-handler (source) (channel)
  (subscribe! (->keyword channel) sock))

(define-file-handler "static")

;;;;;;;;;; Player-info related
(define-handler server-info ()
  (hash :handlers *handlers*
	:public-tables (loop for tbl being the hash-values of (public-tables *server*)
			  collect (obj->hash tbl () id tag player-count max-players))
	:decks (mapcar #'deck-name (decks *server*))
	:minis *mini-uris*
	:tablecloths *tablecloth-uris*))

(define-handler get-session ()
  (with-slots (id tag current-table hand) (lookup :player session)
    (hash :id id :tag tag 
	  :current-table (aif current-table (redact it))
	  :hand hand)))

(define-handler look/table ((table :table)) 
  (redact table))

(define-handler look/hand () 
  (hash-values (hand (lookup :player session))))

(define-handler rename ((new-tag (:string :max 255)))
  (let* ((player (lookup :player session))
	 (old (tag player)))
    (assert-http player)
    (when (string/= new-tag (tag player))
      (setf (tag player) new-tag)
      (publish! (aif (current-table player) it *server*) player :changed-tag `((old-tag . ,old))))
    :ok))

;;;;;;;;;; Lobby actions
(define-handler lobby/speak ((message (:string :min 2 :max 255)))
  (let ((player (lookup :player session)))
    (assert-http player)
    (publish! *server* player :said `((message . ,(escape-string message))))
    :ok))

(define-handler lobby/new-table ((tag :string) (passphrase :string))
  (let ((player (lookup :player session)))
    (assert-http player)
    (with-lock-held ((lock *server*))
      (let ((table (make-instance 'table :tag tag)))
	(insert! *server* table)
	(insert! table player)
	(unless (string= passphrase "")
	  (setf (passphrase table) passphrase)
	  (with-slots (id tag player-count max-players) table
	    (publish! *server* player :started-table `((table ((id . ,id) (tag . ,tag) (seated . ,player-count) (of . ,max-players)))))))
	(redact table)))))

(define-table-handler lobby/join-table ((table :table) (passphrase :string))
  (assert-http (and (lookup :player session) (not (full? table))))
  (with-slots (id players) table
    (let ((player (lookup :player session)))
      (unless (member player (players table))
	(insert! table player)
	(publish! table player :joined `((table . ,(id table))))
	(publish! *server* player
		  (if (full? table) :filled-table :joined)
		  `((id . ,id))))
      (redact table))))

;;;;;;;;;; Table related
;;;;; Direct table actions
(define-player-handler table/save ((table :table))
  (assert-http (= (player-count table) 1))
  (setf (header-out :content-type) "application/json"
	(header-out :content-disposition) "attachment; filename=\"game.json\"")
  (serialize table))

(define-player-handler table/load ((table :table) (file :json-file))
  (assert-http (= (player-count table) 1))
  (let ((player (lookup :player session)))
    (loop for thing in (getj :things file)
       do (case (->keyword (getj :type thing))
	    (:stack 
	     (insert! table (stack<-json player thing)))
	    (:card
	     (insert! table (card<-json player thing)))))
    (publish! table player :loaded `((things . ,(length (getj :things file)))))
    :ok))

(define-player-handler table/leave ((table :table))
  (let ((player (lookup :player session)))

    ;;; TODO -- really, where the orphaned hand is placed should be decided by the front end.
    ;;;         give it some thought.
    (when (> (hash-table-count (hand player)) 0)
      (let ((stack (make-instance 'stack :belongs-to (id player) :x 10 :y 10)))
	(loop for card being the hash-values of (hand player) for count from 1
	   do (setf (card-type stack) (card-type card))
	   do (move! card player stack)
	   finally (setf (card-count stack) count))
	(insert! table stack)))
    ;;;;;;;;;;;;
    
    (delete! table player)
    (setf (current-table player) nil)
    (publish! table player :left)
    (unless (passphrase table)
      (publish! *server* player :left 
		`((table ((id . ,(id table)) (tag . ,(tag table)) (seated . ,(player-count table)) (of . ,(max-players table)))))))
    :ok))

(define-player-handler table/tablecloth ((table :table) (tablecloth-uri :string :max 255))
  (setf (tablecloth table) tablecloth-uri)
  (publish! table (lookup :player session) :tablecloth `((tablecloth . ,tablecloth-uri)))
  :ok)

(define-player-handler table/speak ((table :table) (message (:string :min 2 :max 255)))
  (publish! table (lookup :player session) :said `((message . ,(escape-string message))))
  :ok)

(define-player-handler table/roll ((table :table) (num-dice (:int :min 1 :max 128)) (die-size (:int :min 2 :max 4096)) (modifier (:int :min -4096 :max 4096)))
  (let ((mod (cond ((> modifier 0) (format nil "+~a" modifier))
		   ((> 0 modifier) modifier)
		   (t nil))))
    (multiple-value-bind (total rolls) (roll num-dice die-size)
      (publish! table (lookup :player session) :rolled
		`((dice . ,(format nil "~ad~a~@[~a~]" num-dice die-size mod)) 
		  (total . ,(+ total modifier))
		  (rolls . ,rolls)))))
  :ok)

(define-player-handler table/coin-toss ((table :table))
  (publish! table (lookup :player session) :flipped-coin `((result . ,(pick (list :heads :tails)))))
  :ok)

(define-player-handler table/ping ((table :table) (x :int) (y :int) (z :int))
  (publish! table (lookup :player session) :pinged `((x . ,x) (y . ,y) (z . ,z))))

;;;;; Table element creation handlers
(define-player-handler table/new/mini ((table :table) (image-uri :string :max 255) (x :int) (y :int) (z :int) (rot :int))
  (let ((mini (make-instance 'mini :belongs-to (id (lookup :player session)))))
    (set-props mini image-uri x y z rot)
    (insert! table mini)
    (publish! table (lookup :player session) :placed-mini `((mini . ,(redact mini))))
    :ok))

(define-player-handler table/new/note ((table :table) (text :string :max 255) (x :int) (y :int) (z :int) (rot :int))
  (let ((note (make-instance 'note :belongs-to (id (lookup :player session)))))
    (set-props note x y z rot text)
    (insert! table note)
    (publish! table (lookup :player session) :placed-note `((note . ,(redact note))))
    :ok))

(define-player-handler table/new/note-on ((table :table) (text :string :max 255) (thing :placeable))
  (let ((note (make-instance 'note :belongs-to (id (lookup :player session)) 
			     :attached-to (id thing) :text text)))
    (insert! table note)
    (publish! table (lookup :player session) :placed-note `((note . ,(redact note))))
    :ok))

(define-player-handler table/new/stack-from-cards ((table :table) (cards (:list :card)))
  (let* ((c (first cards))
	 (stack (make-instance 'stack :belongs-to (id (lookup :player session)) :x (x c) :y (y c) :z (z c) :rot (rot c))))
    (loop for card in cards 
       do (delete! table card) do (insert! stack card))
    (publish! table (lookup :player session) :stacked-up `((stack . ,(redact stack)) (cards . ,(mapcar #'id cards))))
    :ok))

(define-player-handler table/new/stack-from-deck ((table :table) (deck-name :string) (x :int) (y :int) (z :int) (rot :int))
  (let ((stack (stack<-deck (lookup :player session) (find-deck deck-name))))
    (set-props stack x y z rot)
    (insert! table stack)
    (publish! table (lookup :player session) :new-deck `((name . ,deck-name) (stack . ,(redact stack))))
    :ok))

(define-player-handler table/new/stack-from-json ((table :table) (deck :json) (x :int) (y :int) (z :int) (rot :int))
  (let ((stack (stack<-json (lookup :player session) deck)))
    (assert-http (cards stack))
    (set-props stack x y z rot)
    (insert! table stack)
    (publish! table (lookup :player session) :new-deck `((name . ,(getj :deck-name deck)) (stack . ,(redact stack))))
    :ok))

(define-player-handler table/play ((table :table) (card (:card :from-hand)) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (set-props card face x y z rot)
  (move! card (lookup :player session) table)
  (publish! table (lookup :player session) :played-from-hand `((card . ,(redact card))))
  :ok)

;;;;; General element actions
(define-player-handler table/remove ((table :table) (thing :placeable))
  (delete! table thing)
  (publish! table (lookup :player session) :removed `((thing . ,(id thing))))
  :ok)

(define-player-handler table/move ((table :table) (thing :placeable) (x :int) (y :int) (z :int) (rot :int))
  (set-props thing x y z rot)
  (publish! table (lookup :player session) :moved  `((thing . ,(id thing)) (x . ,x) (y . ,y) (z . ,z) (rot . ,rot)))
  :ok)

(define-player-handler table/take ((table :table) (thing :placeable))
  (setf (belongs-to thing) (id (lookup :player session)))
  (publish! table (lookup :player session) :took-control `((thing . ,(id thing))))
  :ok)

(define-player-handler table/attach ((table :table) (child :placeable) (parent :placeable))
  (pushnew (id parent) (attached-to child))
  (publish! table (lookup :player session) :attached `((child . ,(id child)) (parent . ,(id parent))))
  :ok)

(define-player-handler table/detach-from ((table :table) (child :placeable) (parent :placeable))
  (setf (attached-to child) (remove (id parent) (attached-to child)))
  (publish! table (lookup :player session) :detached-from `((child . ,(id child)) (parent . ,(id parent))))
  :ok)

(define-player-handler table/detach ((table :table) (child :placeable))
  (setf (attached-to child) nil)
  (publish! table (lookup :player session) :detached `((child . ,(id child))))
  :ok)

;;;;; Stack-specific actions
(define-player-handler table/stack/play-top-card ((table :table) (stack :stack) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (let ((card (pop! stack)))
    (set-props card face x y z rot)
    (insert! table card)
    (publish! table (lookup :player session) :played-from-stack `((stack . ,(id stack)) (card . ,(redact card))))
    :ok))

;; TODO -- there's a few places where I take card-id inputs. Really, those should be :card :in-stack <name of stack>, which should then
;;         look up cards in the named stack and assert that they all exist after the lock. It should be possible to have a list of these too.
;;         This happens in three places, so it'd be worth pulling out into the define-handler language
(define-player-handler table/stack/play ((table :table) (stack :stack) (card-id :keyword) (face :facing) (x :int) (y :int) (z :int) (rot :int))
  (let ((card (find card-id (cards stack) :key #'id)))
    (assert-http card)
    (set-props card face x y z rot)
    (publish! table (lookup :player session) :played-from-stack `((stack . ,(id stack)) (card . ,(redact card))))
    (move! card stack table)
    :ok))

(define-player-handler table/stack/add-to ((table :table) (stack :stack) (card (:card :from-table)))
  (publish! table (lookup :player session) :added-to-stack `((stack . ,(id stack)) (card . ,(id card))))
  (move! card table stack)
  :ok)

(define-player-handler table/stack/merge ((table :table) (stack :stack) (stack-two :stack))
  (dolist (c (cards stack-two)) (insert! stack c))
  (delete! table stack-two)
  (publish! table (lookup :player session) :merged-stacks `((stack . ,(redact stack)) (merged ,(id stack-two))))
  :ok)

(define-player-handler table/stack/shuffle ((table :table) (stack :stack))
  (setf (cards stack) (shuffle (cards stack)))
  (publish! table (lookup :player session) :shuffled `((stack . ,(id stack))))
  :ok)

(define-player-handler table/stack/play-to ((table :table) (card (:card :from-hand)) (stack :stack))
  (setf (face card) :down)
  (move! card (lookup :player session) stack)
  (publish! table (lookup :player session) :played-to-stack `((stack . ,(id stack)) (card . ,(redact card))))
  :ok)

(define-player-handler table/stack/draw ((table :table) (stack :stack) (num :int))
  (loop with rep = (min num (card-count stack)) repeat rep
     do (let ((card (pop! stack)))
	  (insert! (lookup :player session) card)))
  (when (zerop (card-count stack))
    (delete! table stack))
  (publish! table (lookup :player session) :drew-from `((stack . ,(id stack)) (count . ,num)))
  (hash-values (hand (lookup :player session))))

(define-player-handler table/stack/peek ((table :table) (stack :stack) (min (:int :min 0)) (max :int))
  (publish! table (lookup :player session) :peeked `((stack . ,(id stack)) (count . ,(- max min))))
  (take (- max min) (drop min (cards stack))))

(define-player-handler table/stack/take ((table :table) (stack :stack) (card-id :keyword))
  (let ((card (find card-id (cards stack) :key #'id)))
    (assert-http card)
    (move! card stack (lookup :player session))
    (publish! table (lookup :player session) :took-from `((stack . ,(id stack))))
    card))

(define-handler table/stack/reorder ((table :table) (stack :stack) (card-order (:list :keyword)))
  (with-slots (cards) stack
    (let* ((count (length card-order))
	   (head (take count cards))
	   (changed (loop for c-id in card-order
		       for card = (find c-id head :key #'id)
		       do (assert-http card) collect card)))
      (setf cards (append changed (drop count cards)))))
  (publish! table (lookup :player session) :reordered `((stack . ,(id stack)) (count . ,(length card-order))))
  :ok)

;;;;; Card-specific actions
(define-player-handler table/card/flip ((table :table) (card (:card :from-table)))
  (setf (face card) (if (eq (face card) :up) :down :up))
  (publish! table (lookup :player session) :flipped `((card . ,(redact card))))
  :ok)

(define-player-handler table/card/pick-up ((table :table) (card (:card :from-table)))
  (move! card table (lookup :player session))
  (publish! table (lookup :player session) :picked-up `((card . ,(id card))))
  (hash-values (hand (lookup :player session))))

;;;;; Note-specific actions
(define-player-handler table/note/change ((table :table) (note :note) (new-text :string :max 255))
  (let ((old-text (text note)))
    (unless (string= new-text old-text)
      (setf (text note) new-text)
      (publish! table (lookup :player session) :changed-note `((note . ,(id note)) (old . ,old-text) (new . ,new-text))))
    :ok))