;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;; JS
(to-file "static/js/lobby.js"
	 (ps
	   (define-component (lobby) 
	       (:div :id "lobby"
		     (:div :id "left-pane")
		     (:div :id "right-pane"
			   (:div :id "player-info"
				 (:span :class "player-id")
				 (:span :class "player-tag"))
			   (:ul :id "open-tables")
			   (:ul (:li (:button :id "new-table" "New Table"))))
		     (:div :id "new-table-setup" :class "overlay"
			   (:h3 "New Table")
			   (:div :class "content"
				 (:input :class "game-tag")
				 (:button :class "ok" "Ok")
				 (:button :class "cancel" "Cancel"))))
	     (when *table-stream* 
	       (chain *table-stream* (close))
	       (setf *table-stream* nil))
	     (show-chat "#left-pane")
	     ($click "#new-table-setup .ok" (lobby/new-table ($ "#new-table-setup .game-tag" (val)) "")
		     "#new-table-setup .cancel" ($ "#new-table-setup" (hide))
		     "#new-table" (progn ($ "#new-table-setup" (show))
					 ($ "#new-table-setup .game-tag" (focus))))
	     ($keydown "#new-table-setup .game-tag" 
		       <ret> ($ "#new-table-setup .ok" (click))
		       <esc> ($ "#new-table-setup .cancel" (click)))
	     (server-info))
	   
	   (defun render-table-entry (tbl-entry)
	     (with-slots (id tag player-count max-players) tbl-entry
	       ($prepend "#open-tables"
			 (:li :id (+ "game-" id) 
			      (:span :class "tag" tag)
			      (:span :class "id" id) 
			      (:span :class "players" (:span :class "count" player-count) "/" max-players)
			      (:button :class "join" "Join")))
	       ($click (+ "#game-" id " .join") (lobby/join-table id ""))))))

(to-file "static/js/chat.js"
	 (ps (define-component (chat)
		 (:div :class "chat"
		       (:ul :id "chat-history")
		       (:div :id "chat-controls"
			     (:textarea :id "chat-input" :type "text")
			     (:button :id "send" :class "chat-button" "Send")))
	       ($keydown "#chat-input"
			 <ret> (unless shift?
				 (chain event (prevent-default))
				 ($ "#send" (click)))
			 <up> (when ctrl?
				(with-slots (messages current-message) *chat-history*
				  (setf current-message (max 0 (- current-message 1)))
				  ($ "#chat-input" (val (aref messages current-message)))))
			 <down> (when ctrl?
				  (with-slots (messages current-message) *chat-history*
				    (setf current-message (min (length messages) (+ current-message 1)))
				    ($ "#chat-input" (val (aref messages current-message)))))
			 <esc> ($ "#chat-input" (val "")))
	       ($click "#lobby .chat #send"
		       (let ((txt ($ "#chat-input" (val))))
			 (push-chat-message txt)
			 (lobby/speak txt))
		       "#table-toolbar #send" 
		       (let ((txt ($ "#chat-input" (val))))
			 (push-chat-message txt)
			 (if (chain txt (match "^@"))
			     (chat-command txt)
			     (table/speak txt)))))

	     (defun chat-message (msg)
	       (with-slots (player player-tag time type message) msg
		 (who-ps-html (:li :class (if (= type "said") "said" "did")
				   (:span :class "time" (new (chain (-date time) (to-time-string))))
				   (:span :class "player" player)
				   (:span :class "player-tag" player-tag)
				   (:div :class "message"
					 (case type
					   ("joined" 
					    (+ "joined table " (@ msg table)))
					   ("left" 
					    "left the table")
					   ("said" 
					    (newline->break message))
					   ("loaded"
					    (+ "loaded a saved table. Added " (@ msg things) " things."))
					   ("tablecloth"
					    (+ "set a new tablecloth"))
					   ("moved"
					    (+ "moved " (@ msg thing)))
					   ("changedTag"
					    (+ "changed their tag from '" (@ msg old-tag) "'"))
					   ("startedTable"
					    (+ "started table " (@ msg id) (if (@ msg tag) (+ ", '" (@ msg tag) "'") "")))
					   ("filledTable"
					    "filled up table " (@ msg id) (if (@ msg tag) (+ ", '" (@ msg tag) "'") ""))
					   ("newDeck" 
					    (+ "put down a '" (@ msg name) "' deck"))
					   ("drewFrom" 
					    (+ "drew " (@ msg count) " from " (@ msg stack)))
					   ("peeked"
					    (+ "peeked at " (@ msg count) " cards from " (@ msg stack)))
					   ("revealed"
					    (+ "revealed some cards"))
					   ("flipped"
					    (+ "flipped over " (chat-card (@ msg card))))
					   ("placedMini" "placed a mini")
					   ("removed"
					    (+ "removed " (@ msg thing) " from play"))
					   ("playedFromHand"
					    (+ "played " (chat-card (@ msg card)) " from hand"))
					   ("playedFromStack"
					    (+ "played " (chat-card (@ msg card)) " from " (@ msg stack)))
					   ("playedToStack"
					    (+ "played " (chat-card (@ msg card)) " to stack " (@ msg stack)))

					   ("pinged"
					    (+ "pinged " (@ msg x) "." (@ msg y)))
					   ("placedNote"
					    (+ "placed a note"))
					   ("attachedNote"
					    (+ "attached note " (@ msg note) " to " (@ msg thing)))
					   ("addedToStack"
					    (+ "put " (@ msg card) " on top of " (@ msg stack)))
					   ("mergedStacks"
					    (+ "merged " (@ msg merged) " into " (@ msg stack id)))
					   ("shuffled"
					    (+ "shuffled " (@ msg stack)))
					   ("pickedUp"
					    (+ "picked up card " (@ msg card)))
					   ("rolled"
					    (+ "rolled " (@ msg total) " on " (@ msg dice) "; " (obj->string (@ msg rolls))))
					   ("flippedCoin"
					    (+ "flipped a coin; it landed on " (@ msg result)))
					   (t "did something")))))))
	     
	     (defun update-chat (selector msg-html)
	       (let ((scrl? (scrolled-to-bottom? selector)))
		 ($ selector (append msg-html))
		 (when scrl? (scroll-to-bottom selector))))

	     (defun push-chat-message (message)
	       (let* ((re (new (-reg-exp "\\s+$")))
		      (msg (chain message (replace re ""))))
		 (with-slots (messages current-message) *chat-history*
		   (setf current-message (length messages))
		   (unless (= msg (aref messages (- current-message 1)))
		     (chain messages (push msg))
		     (set-cookie :chat-messages messages)
		     (if (> (length messages) 50)
			 (chain messages (splice 0 1))
			 (incf current-message))))))

	     (defun chat-command (txt)
	       (let ((re (new (-reg-exp "^(@\\S+)"))))
		 (awhen (chain txt (match re))
		   ((aref *chat-commands* (@ it 1))
		    (chain txt (substring (length (@ it 1))) (replace " +" "")))
		   ($ "#chat-input" (val ""))
		   t)))
	     
	     (defvar *chat-commands*
	       (create "@roll"   (lambda (txt) (string->die-roll txt))
		       "@toss"   (lambda (txt) (table/coin-toss))
		       "@rename" (lambda (txt) (rename txt))))

	     (defun chat-card (card)
	       (who-ps-html
		(who-ps-html (:div :class "card in-chat" 
				   (:span :class "content" (card-html card))))))))

(to-file "static/js/table.js"
	 (ps
	   (define-component (table)
	       (:div :id "table"
		(:div :id "board" :style (aif (@ *table-info* tablecloth) (+ "background-image: url(" it ")") ""))
		(:div :id "zoomed-card" :class "moveable"
		      (:h3 "Zoomed" (:button :class "hide"))
		      (:div :class "content"))
		(:div :id "table-toolbar" :class "moveable"		      
		      (:h3 (:span :class "game-id" (@ *table-info* id))
			   (:span :class "game-tag" (@ *table-info* tag)))
		      (:div :id "player-info" :class "control-row"
			    (:span :class "player-id" (@ *session* id))
			    (:span :class "player-tag" (@ *session* tag)))
		      (:div :class "control-row"
			    (:button :id "leave" "Leave Table")
			    (:button :id "save-board" "Save")
			    (:form :id "load-form" :enctype "multipart/form-data"
				   (:span :class "label" "Load: ") 
				   (:input :type "hidden" :name "table" :value (@ *table-info* id))
				   (:input :name "file" :type "file")))
		      (:div :class "content"
			    (:h2 "Hand")
			    (:div :id "hand")
			    (:h2 "Chat")
			    (:div :id "game-chat")
			    (:div :id "backpack"
				  (:ul
				   (:li (:a :href "#decks-tab" "Decks") (:button :id "custom-deck" "Custom Deck"))
				   (:li (:a :href "#dice-tab" "Dice"))
				   (:li (:a :href "#minis-tab" "Minis") (:button :id "custom-mini" "Custom Tablecloth"))
				   (:li (:a :href "#tablecloth-tab" "Tablecloths") (:button :id "custom-tablecloth" "Custom Tablecloth")))
				  (:div :id "decks-tab" 
					(:div :class "content"
					      (map-markup (@ *server-info* decks) (:div :class "new-deck" elem)))
					(:br :class "clear"))
				  (:div :id "dice-tab" 
					(:div :class "content"
					      (map-markup (list "d3" "d4" "d6" "d8" "d10" "d20" "d100")
							  (:div :class "die-roll-icon" 
								(:span :class "num-dice" "1")
								elem
								(:button :class "increment")
								(:button :class "decrement")))
					      (:div :class "coin-flip-icon" "Flip"))
					(:br :class "clear"))
				  (:div :id "minis-tab"
					(:div :class "content"
					      (map-markup 
					       (@ *server-info* minis)
					       (:img :class "backpack-mini" :src elem)))
					(:br :class "clear"))
				  (:div :id "tablecloth-tab" 
					(:div :class "content"
					      (:div :class "tablecloth" :title "" "None")
					      (map-markup 
					       (@ *server-info* tablecloths)
					       (:div :class "tablecloth" :title elem
						     :style (+ "background-image: url(" elem ");")
						     (uri->name elem))))
					(:br :class "clear"))))))
	     (when *lobby-stream* 
	       (chain *lobby-stream* (close))
	       (setf *lobby-stream* nil))

	     (show-deck-editor "#table")

	     ($draggable ".tablecloth" (:revert t))
	     ($draggable ".backpack-mini" (:revert t))

	     ($droppable "#backpack" (:overlapping "#board, .stack")
			 (:mini  (table/remove ($ dropped (attr :id))))
			 (:stack (table/remove ($ dropped (attr :id))))
			 (:card  (table/remove ($ dropped (attr :id)))))
	     
	     ($ "#load-form" (change (fn ($upload "#load-form" "/table/load"))))

	     ($map (@ *session* cookie)
		   (when (chain i (match "^#"))
		     ($ i (offset (string->obj elem)))))

	     ($button "#zoomed-card button.hide" (:zoomout) ($ "#zoomed-card" (hide)))

	     (show-chat "#game-chat")
	     ($button ".die-roll-icon .increment" (:plus)
		      (let ((trg ($ this (siblings ".num-dice"))))
			($ trg (text (min 4096 (+ 1 ($int trg)))))
			(set-dice-cookie)))
	     ($button ".die-roll-icon .decrement" (:minus)
		      (let ((trg ($ this (siblings ".num-dice"))))
			($ trg (text (max 1 (- ($int trg) 1))))
			(set-dice-cookie)))

	     (aif (@ *session* cookie :dice)
		  (loop for elem in ($ "#dice-tab .num-dice") for num-dice in (string->obj it)
		     do ($ elem (text num-dice))))

	     (defun set-dice-cookie ()
	       (set-cookie :dice
			   (loop for elem in ($ "#dice-tab .num-dice")
			      collect ($ elem (text)))))

	     ($click "#save-board"
		     (with-slots (id tag) *table-info*
		       ($post "/table/save" (:table id)
			      ($save-as (name->filename tag) res))))
	     
	     ($droppable "#hand" (:overlapping "#board, .stack")
			 (:card (unless ($ dropped (has-class :card-in-hand))
				  (table/card/pick-up ($ dropped (attr :id))))))
	     
	     
	     
	     ($draggable ".moveable" (:handle "h3")
			 (set-cookie (+ "#" ($ this (attr :id))) ($ this (offset))))
	     ($draggable ".new-deck" (:revert t))
	     ($draggable ".die-roll-icon, .coin-flip-icon" (:revert t :cancel ".increment, .decrement"))

	     ($append "body" 
		      (:div :class "dialog" :id "custom-tablecloth-dialog" 
			    (:input :class "url-input" :placeholder "Tablecloth image URL")
			    (:input :class "name-input" :placeholder "Tablecloth image URL")
			    (:button :class "ok" "Ok"))
		      (:div :class "dialog" :id "custom-mini-dialog" 
			    (:input :class "url-input" :placeholder "Mini image URL")
			    (:button :class "ok" "Ok")))
	     ($click "#custom-tablecloth-dialog .ok" 
		     (let ((uri ($ "#custom-tablecloth-dialog .url-input" (val)))
		     	   (name ($ "#custom-tablecloth-dialog .name-input" (val))))
		       ($append "#tablecloth-tab .content"
		     		(:div :class "tablecloth" :title uri
		     		      :style (+ "background-image: url(" uri ");")
		     		      name))
		       ($ "#custom-tablecloth-dialog input" (val ""))
		       ($ "#custom-tablecloth-dialog" (dialog "close"))
		       ($draggable ".tablecloth:last" (:revert t)))
		     "#custom-mini-dialog .ok" 
		     (let ((uri ($ "#custom-mini-dialog .url-input" (val))))
		       ($append "#minis-tab .content" (:img :class "backpack-mini" :src uri))
		       ($ "#custom-mini-dialog .url-input" (val ""))
		       ($ "#custom-mini-dialog" (dialog "close"))
		       ($draggable ".backpack-mini:last" (:revert t))))

	     ($ ".dialog" (dialog (create "autoOpen" false)))

	     ($ "#backpack" (tabs))
	     ($button "#custom-deck" (:plus) 
		      ($ "#deck-editor" (show))
		      ($ "#backpack" (tabs (create :active 0))))
	     ($button "#custom-mini" (:plus) 
		      ($ "#deck-editor, .dialog" (hide))
		      ($ "#custom-mini-dialog" (dialog :open))
		      ($ "#backpack" (tabs (create :active 2))))
	     ($button "#custom-tablecloth" (:plus) 
		      ($ "#deck-editor, .dialog" (hide))
		      ($ "#custom-tablecloth-dialog" (dialog :open))
		      ($ "#backpack" (tabs (create :active 3))))

	     ($click "#leave" (table/leave))
	     
	     (setf *table-stream*
		   (event-source (+ "/ev/" (chain (@ *table-info* id) (to-upper-case)))
				 (joined)
				 (left)
				 (said)
				 (pinged
				  ($append "body" (:div :class "ping" :style (+ "left:" (- (@ ev x) 15) "px; top:" (- (@ ev y) 15) "px;")))
				  ($ ".ping" (stop t t) (effect :highlight nil 500 (fn ($ ".ping" (remove))))))

				 ;;; todo
				 (placed-note)
				 (attached-note)

				 (loaded (look/table))
				 (tablecloth
				  ($ "#board" (css "background-image" (+ "url(" (@ ev tablecloth) ")"))))
				 (changed-tag)
				 (moved 
				  (with-slots (thing x y) ev
				    (let ((elem ($ (+ "#" thing))))
				      ($ elem 
					 (offset (create :left x :top y))
					 (css "z-index" (+ y ($ elem (height))))))))
				 (took-control (log "Someone took something"))
				 (flipped 
				  ($ (+ "#" (@ ev card id)) (remove))
				  (case (@ ev card type)
				    ("card" (create-card "body" (@ ev card)))
				    ("stack" (create-stack "body" (@ ev card)))))
				 (new-deck 
				  (create-stack "body" (@ ev stack)))
				 (stacked-up (log "Made a stack from cards"))
				 (merged-stacks 
				  ($ (+ "#" (@ ev merged)) (remove))
				  ($ (+ "#" (@ ev stack id)) (remove))
				  (create-stack "body" (@ ev stack)))
				 (added-to-stack 
				  ($ (+ "#" (@ ev card)) (remove))
				  (change-stack-count (@ ev stack) +1)
				  ($ (+ "#" (@ ev stack)) (highlight))
				  (log "Put a card onto a stack"))
				 (drew-from 
				  (change-stack-count (@ ev stack) -1)
				  ($highlight (+ "#" (@ ev stack))))
				 (shuffled
				  ($highlight (+ "#" (@ ev stack))))

				 (peeked (log "Peeked at cards from a stack"))
				 (revealed (log "Showed everyone cards from a stack"))

				 (placed-mini
				  (create-mini "body" (@ ev mini)))
				 (removed
				  ($ (+ "#" (@ ev thing)) (remove)))
				 (played-from-hand 
				  (create-card "body" (@ ev card)))

				 (played-from-stack (log "Played the top card from a stack"))
				 (played-to-stack
				  (change-stack-count (@ ev stack) +1)
				  (log "Played to the top of a stack"))
				 (picked-up 
				  ($ (+ "#" (@ ev card)) (remove))
				  (log "Picked up a card"))

				 (rolled (log "Rolled"))
				 (flipped-coin (log "Flipped a coin")))))

	   (defun render-board (table)
	     (let ((board-selector "#board")
		   (chat-selector "#chat-history")
		   (ts (@ table things)))
	       ($ board-selector (empty))

	       ($ "body"
		  (on :click
		      (lambda (event)
			(when (@ event ctrl-key)
			  (table/ping (@ event page-x) (@ event page-y) 0)))))

	       (awhen (@ table tablecloth)
		 ($ board-selector (css "background-image" (+ "url(" it ")"))))
	       ($droppable board-selector ()
			   (:card-in-hand 
			    (table/play ($ dropped (attr :id)) (if shift? :down :up) ev-x ev-y 0 0))
			   
			   (:new-custom-deck
			    (table/new/stack-from-json
			     (obj->string (aref (@ *session* cookie :custom-decks) ($ dropped (text))))
			     ev-x ev-y 0 0))
			   
			   (:backpack-mini
			    (table/new/mini ($ dropped (attr :src)) ev-x ev-y 0 0))

			   (:new-deck
			    (table/new/stack-from-deck ($ dropped (text)) ev-x ev-y 0 0))

			   (:tablecloth (table/tablecloth ($ dropped (attr :title))))

			   (:die-roll-icon
			    (string->die-roll ($ dropped (text))))
			   (:coin-flip-icon
			    (table/coin-toss)))
	       ($ chat-selector 
		  (empty) 
		  (append ($map (@ table history)
				($ chat-selector (prepend (chat-message elem))))))
	       (scroll-to-bottom chat-selector)
	       ($map ts 
		     (cond ((= (@ elem type) :stack) (create-stack "body" elem))
			   ((= (@ elem type) :card) (create-card "body" elem))
			   ((= (@ elem type) :mini) (create-mini "body" elem))))))
	   
	   (defun render-hand (cards)
	     (let ((hand-selector "#hand"))
	       ($ hand-selector (empty))
	       ($map cards (create-card-in-hand hand-selector elem))))

	   (define-thing stack
	       (:div :id (self id) :class "stack" :style (self position)		     
		     (:button :class "draw" "Draw")
		     (:button :class "shuffle")
		     (:div :class "card-count" (+ "x" (self card-count))))
	     ($ $self (css "z-index" (+ (self y) ($ $self (height)))))
	     ($draggable $self () (table/move (self id) (@ ui offset left) (@ ui offset top) 0 0))
	     ($droppable $self (:overlapping "#board")
			 (:card-in-hand
			  (table/stack/play-to ($ dropped (attr :id)) (self id)))
			 (:card
			  (table/stack/add-to (self id) ($ dropped (attr :id))))
			 (:stack
			  (table/stack/merge (self id) ($ dropped (attr :id)))))
	     ($button ($child ".shuffle") (:shuffle) (table/stack/shuffle (self id)))
	     ($click ($child ".draw") (table/stack/draw (self id) 1)))

	   (define-thing mini
	       (:img :id (self id) :class "mini" :style (self position) :src (self mini-uri))
	     ($ $self (css "z-index" (+ (self y) ($ $self (height)))))
	     ($draggable $self () (table/move (self id) (@ ui offset left) (@ ui offset top) 0 0)))

	   (define-thing card 
	       (:div :id (self id) :class (+ "card " (self card-type)) :style (self position)
		     (:span :class "content" (card-html self))
		     (:span :class "card-type" (self card-type))
		     (:button :class "zoom"))
	     ($ $self (css "z-index" (+ (self y) ($ $self (height)))))
	     ($button ($child ".zoom") (:zoomin)
		      ($ "#zoomed-card" (toggle))
		      ($ "#zoomed-card .content" (empty) (append (card-html self))))	     
	     ($draggable $self () 
			 (table/move (self id) (@ ui offset left) (@ ui offset top) 0 0)
			 (when shift? (table/card/flip (self id)))))

	   (define-thing card-in-hand
	       (:div :id (self id) :class "card card-in-hand"
		     (:span :class "content" (card-html self))
		     (:button :class "zoom"))
	     ($button ($child ".zoom") (:zoomin)
		      ($ "#zoomed-card" (show))
		      ($ "#zoomed-card .content" (empty) (append (card-html self))))
	     ($draggable $self (:revert t)))))

(to-file "static/js/deck-editor.js"
	 (ps 
	   (define-component (deck-editor :empty? nil)
	       (:div :id "deck-editor" :class "moveable"
		     (:h3 "Deck Editor")
		     (:div :class "content"
			   (:div :class "row"
				 (:form :id "load-deck-form" :enctype "multipart/form-data"
					(:span :class "label" "Load: ") (:input :name "deck" :type "file")))
			   (:div :class "row"
				 (:span :class "label" "Deck Name: ")
				 (:input :class "deck-name"))
			   (:div :class "row"
				 (:span :class "label" "Card Type: ")
				 (:input :class "card-type"))
			   (:div :class "row"
				 (:ul :class "cards"))
			   (:div :class "row"
				 (:textarea :class "new-card"))
			   (:div :class "row"
				 (:button :class "add-card" "Add Card"))
			   (:div :class "row"
				 (:button :class "ok" "Ok")
				 (:button :class "cancel" "Cancel"))))
	     
	     (defun cookie-decks ()
	       (set-cookie :custom-decks (@ *session* cookie :custom-decks)))

	     (defun load-deck-for-editing (deck)
	       (with-slots (deck-name card-type cards) deck
		 ($ "#deck-editor .deck-name" (val deck-name))
		 ($ "#deck-editor .card-type" (val card-type))
		 (loop for c in cards do (create-card-record "#deck-editor .cards" c))))
	     
	     ($ "#load-deck-form" 
		(change 
		 (fn ($upload "#load-deck-form" "/load-deck"
			      (load-deck-for-editing res)))))
	     
	     (define-thing card-record
		 (:li (:button :class "add")(:button :class "remove")
		      (:span :class "content" (obj->string self)))
	       ($button ($child ".remove") (:minus))
	       ($button ($child ".add") (:plus)))

	     (define-thing custom-deck
		 (:div :class "new-deck new-custom-deck"
		       :title (self deck-name) (self deck-name)
		       (:button :class "delete")
		       (:button :class "edit")
		       (:button :class "download"))
	       ($button ($child ".delete") (:cancel)
			(delete (aref (@ *session* cookie :custom-decks) (self deck-name)))
			;; (cookie-decks)
			($ $self (remove)))
	       ($button ($child ".download") (:arrowthick-1-s)
			($save-as (name->filename (self deck-name)) self))
	       ($button ($child ".edit") (:pencil)
			(load-deck-for-editing self)
			($ "#deck-editor" (show)))
	       ($draggable $self (:revert t)))
	     
	     ;; get custom decks from cookie
	     (aif (@ *session* cookie :custom-decks)
		  (let ((decks (string->obj it)))
		    (setf (@ *session* cookie :custom-decks) decks)
		    ($map decks (create-custom-deck "#decks-tab .content" elem)))
		  (setf (@ *session* cookie :custom-decks) (create)))

	     ($on "#deck-editor"
		  (:click "button.remove" ($ this (parent) (remove)))
		  (:click "button.add" ($ "#deck-editor .cards" (append ($ this (parent) (clone)))))
		  (:keydown ".new-card"
			    <ret> (unless shift?
				    (chain event (prevent-default))
				    ($ "#deck-editor button.add-card" (click)))
			    <esc> ($ "#deck-editor .new-card" (val ""))))

	     ($click "#deck-editor button.add-card"
		     (let ((txt ($ "#deck-editor .new-card" (val))))
		       (create-card-record "#deck-editor .cards" (try (string->obj txt) (:catch (error) txt)))
		       ($ "#deck-editor .new-card" (val "")))

		     "#deck-editor button.cancel"
		     ($ "#deck-editor" (hide))

		     "#deck-editor button.ok"
		     (let* ((deck-name ($ "#deck-editor .deck-name" (val)))
			    (card-type ($ "#deck-editor .card-type" (val)))
			    (deck (create 'deck-name deck-name 
					  'card-type card-type 
					  'cards (loop for card-elem in ($ "#deck-editor .cards .content")
						    collect (let ((txt ($ card-elem (text))))
							      (try (string->obj txt) (:catch (error) txt)))))))
		       (setf (aref (@ *session* cookie :custom-decks) deck-name) deck)
;;		       (cookie-decks)
		       (when ($exists? (+ ".new-deck.new-custom-deck[title='" deck-name "']"))
			 ($ (+ ".new-deck.new-custom-deck[title='" deck-name "']") (remove)))
		       (create-custom-deck "#decks-tab .content" deck)
		       ($draggable "#decks-tab .new-custom-deck:first" (:revert t))
		       ($ "#deck-editor" (hide)))))))

(to-file "static/js/util.js"
	 (ps 
	   (defun name->filename (name &optional (extension ".json"))
	     (+ (chain name (replace " " "-") (to-lower-case)) extension))
	   
	   (defun uri->name (uri)
	     (chain 
	      (loop for word in (chain uri (match "/([^/]+)\\.") 1 (split "-"))
		 collect (capitalize word))
	      (join " ")))
	   
	   (defun capitalize (string)
	     (+ (chain string (char-at 0) (to-upper-case))
		(chain string (slice 1))))
	   
	   (defun set-cookie (name value &optional (expires 120))
	     (setf (@ document cookie) (encode-cookie name value expires)))
	   
	   (defun get-cookies ()
	     (decode-cookies (@ document cookie)))

	   (defun encode-cookie (name value &optional (expires 120))
	     (let ((d (new (-date))))
	       (chain d (set-date (+ (chain d (get-date)) expires)))
	       (+ "" name "=" (escape (if (stringp value) value (obj->string value)))
		  "; expires=" (chain d (to-u-t-c-string)))))

	   (defun decode-cookies (cookie)
	     (let ((props (chain cookie (split "; ")))
		   (res (create)))
	       (loop for p in props for sp = (chain p (split "="))
		  do (setf (aref res (@ sp 0)) (unescape (@ sp 1))))
	       res))
	   
	   (defun change-stack-count (stack-id by)
	     (let* ((id (+ "#" stack-id))
		    (ct-class (+ id " .card-count"))
		    (count ($int ct-class 1)))
	       (if (= count (- by))
		   ($ id (remove))
		   ($ ct-class (html (+ "x" (+ count by)))))))

	   (defun string->die-roll (die-string)
	     (let-match die-string "(\\d*)d(\\d+)([-+]\\d+)?"
			((num-dice "1")
			 die-size
			 (modifier "0"))
			(table/roll num-dice die-size modifier)))

	   (defun scrolled-to-bottom? (selector)
	     (let ((sel ($ selector)))
	       (when (>= (+ (chain sel (scroll-top)) (chain sel (inner-height)))
			 (@ sel 0 scroll-height))
		 t)))

	   (defun scroll-to-bottom (selector)
	     (let ((sel ($ selector)))
	       (chain sel (scroll-top (@ sel 0 scroll-height)))))

	   (defun newline->break (message)
	     (let ((re (new (-reg-exp #\newline :g))))
	       (chain message (replace re "<br />"))))	   

	   (defun card-html (card)
	     (markup-by-card-type card
				  ("french"
				   (:div (@ content rank)
					 (case (@ content suit)
					   ("hearts" (who-ps-html (:span :style "color: red;" "&#9829;")))
					   ("spades" (who-ps-html (:span :style "color: black;" "&#9824;")))
					   ("diamonds" (who-ps-html (:span :style "color: red;" "&#9830;")))
					   ("clubs" (who-ps-html (:span :style "color: black;" "&#9827;")))))
				   (:div "Face-Down French"))
				  ("nItalian"
				   (:div (@ content rank) " - " (@ content suit))
				   (:div "Face-Down Italian"))
				  ("occultTarot"
				   (:div (@ content rank) " T " (@ content suit))
				   (:div "Face-Down Tarot"))))))

(to-file "static/js/deal.js"
	 (ps (defvar *server-info* nil)
	     (defvar *table-info* nil)

	     (defvar *session* nil)
	     (defvar *chat-history* 
	       (create messages (new (-array))
		       current-message 0))
	     
	     (defvar *lobby-stream* nil)
	     (defvar *table-stream* nil)
	     
 	     (doc-ready 
	      (show-lobby "body")
	      ($on "body"
		   (:click "#player-info span.player-tag"
			   ($replace this (:input :class "player-tag" :value (@ *session* tag)))
			   ($keydown "#player-info input.player-tag"
				     <esc> ($replace this (:span :class "player-tag" (@ *session* tag)))
				     <ret> (rename ($ this (val))))
			   ($ "#player-info input.player-tag" (focus)))))
	     

	     ;;;;;;;;;; Client-side handler definitions
	     ;;;;; General getters
	     (define-ajax server-info ()
	       (with-slots (handlers decks public-tables) res
		 (setf *server-info* res
		       *lobby-stream*
		       (event-source "/ev/lobby"
				     (said)
				     (changed-tag)
				     (started-table
				      (render-table-entry (@ ev table)))
				     (filled-table
				      ($ (+ "#game-" (@ ev id)) (remove)))
				     (joined
				      (let* ((elem ($ (+ "#game-" (@ ev id) " .players .count")))
					     (new-count (+ 1 ($int elem))))
					(chain elem (text (+ 1 ($int elem))))))
				     (left
				      (let ((sel (+ "#game-" (@ ev id))))
					(if ($exists? sel)
					    ($ sel (replace (render-table-entry (@ ev table))))
					    (render-table-entry (@ ev table)))))))
		 ($map public-tables
		       (with-slots (player-count max-players) elem
			 (when (< player-count max-players) (render-table-entry elem)))))
	       (get-session))

	     (define-ajax get-session ()
	       (setf *session* res
		     (@ *session* cookie) (get-cookies))
	       ($ "#player-info .player-id" (text (@ *session* id)))

	       ;; get tag from cookie
	       (aif (@ *session* cookie tag)
		    (progn (log "COOKIED TAG: " it)
			   (rename it))
		    ($ "#player-info .player-tag" (text (@ *session* tag))))

	       ;; get chat history from cookie
	       (aif (@ *session* cookie :chat-messages)
		    (setf (@ *chat-history* messages) (string->obj it)
			  (@ *chat-history* current-message) (length (@ *chat-history* messages))))

	       (when (@ res current-table)
		 (setf *table-info* (@ res current-table))
		 (show-table "body")		  
		 (look/hand)
		 (render-board (@ res current-table))))

	     (define-ajax look/table () (render-board res))
	     (define-ajax look/hand () (render-hand res))
	     
	     (define-ajax rename (new-tag)
	       (setf (@ *session* tag) new-tag)
	       (unless (= new-tag (@ *session* cookie tag))
		 (setf (@ *session* cookie tag) new-tag)
		 (set-cookie :tag new-tag))
	       ($ "#player-info .player-tag" (text new-tag))
	       ($replace "input.player-tag" (:span :class "player-tag" new-tag)))

	     ;;;;; Lobby actions
	     (define-ajax lobby/speak (message)
	       ($ "#chat-input" (val "")))

	     (define-ajax lobby/new-table (tag passphrase)
	       (setf *table-info* res)
	       (show-table "body")
	       (look/hand)
	       (render-board res))

	     (define-ajax lobby/join-table (table passphrase)
	       (log "JOINING TABLE" res)
	       (setf *table-info* res)
	       (show-table "body")		  
	       (look/hand)
	       (render-board res))

	     ;;;;; Table actions
	     ;;; General table actions (load/save involve uploading, so must be inlined)
	     (define-ajax table/leave ()
	       (log "LEAVING TABLE")
	       (show-lobby "body"))
	     
	     (define-ajax table/tablecloth (tablecloth-uri))
	     (define-ajax table/speak (message) ($ "#chat-input" (val "")))
	     (define-ajax table/roll (num-dice die-size modifier))
	     (define-ajax table/coin-toss ())
	     (define-ajax table/ping (x y z))

	     ;;; Table element placement actions
	     (define-ajax table/new/mini (mini-uri x y z rot))
	     ;; TODO table/new/note
	     ;; TODO table/new/note-on
	     (define-ajax table/new/stack-from-deck (deck-name x y z rot))
	     (define-ajax table/new/stack-from-cards (cards))
	     (define-ajax table/new/stack-from-json (deck x y z rot))
	     (define-ajax table/play (card face x y z rot) ($ (+ "#" card) (remove)))
	     
	     ;;; General element actions
	     (define-ajax table/remove (thing))
	     (define-ajax table/move (thing x y z rot))
	     ;; TODO table/take

	     ;;; Stack-related actions
	     ;; TODO table/stack/play
	     (define-ajax table/stack/add-to (stack card))
	     (define-ajax table/stack/merge (stack stack-two))
	     (define-ajax table/stack/shuffle (stack))
	     (define-ajax table/stack/play-to (card stack) ($ (+ "#" card) (remove)))
	     (define-ajax table/stack/draw (stack num) (render-hand res))
	     ;; TODO table/stack/peek
	     ;; TODO table/stack/show

	     ;;; Note-related actions
	     ;; TODO table/note/attach

	     ;;; Card-related actions
	     (define-ajax table/card/flip (card))
	     (define-ajax table/card/pick-up (card)
	       ($ (+ "#" card) (remove))
	       (render-hand res))))

;;;;; HTML
(to-file "static/index.html"
	 (html-str
	   (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
		  (:head (:title "Tabletop Prototyping System - Deal")
			 (styles "jquery-ui.css" "main.css")
			 (scripts "jquery-2.0.3.min.js" "jquery-ui-1.10.3.custom.min.js" 
				  "Blob.js" "FileSaver.js" 
				  "util.js" "chat.js" "lobby.js" "deck-editor.js" "table.js" "deal.js"))
		  (:body))))