;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;; JS
(to-file "static/js/lobby.js"
	 (ps
	   (define-component lobby 
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
			   (:div :class "body"
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
	       ($click (+ "#game-" id " .join") (lobby/join-table id ""))))

	   (define-component chat
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
			   (play/speak txt)))))))

(to-file "static/js/table.js"
	 (ps
	   (define-component table
	       (:div
		(:div :id "board")
		(:div :id "zoomed-card" :class "moveable"
		      (:h3 "Zoomed" (:button :class "hide"))
		      (:div :class "content"))
		(:div :id "table-toolbar" :class "moveable"
		      (:h3 (:span :class "game-id" *current-table-id*)
			   (:span :class "game-tag" *current-table-tag*))
		      (:div :id "player-info" :class "control-row"
			    (:span :class "player-id" (@ *session* id))
			    (:span :class "player-tag" (@ *session* tag)))
		      (:div :class "control-row"
			    (:button :id "leave" "Leave Table")
			    (:button :id "custom-deck" "Custom Deck")
			    (:button :id "save-board" "Save")
			    (:form :id "load-form" :enctype "multipart/form-data"
				   (:span :class "label" "Load: ") 
				   (:input :type "hidden" :name "table" :value *current-table-id*)
				   (:input :name "file" :type "file")))
		      (:div :class "contents"
			    (:h2 "Hand")
			    (:div :id "hand")
			    (:h2 "Chat")
			    (:div :id "game-chat")
			    (:div :id "backpack"
				  (:ul
				   (:li (:a :href "#decks-tab" "Decks"))
				   ;; (:li (:a :href "#minis-tab" "Minis"))
				   (:li (:a :href "#dice-tab" "Dice/Coin")))
				  (:div :id "decks-tab" 
					(map-markup *decks-list* (:div :class "new-deck" elem))
					(:br :class "clear"))
				  ;; (:div :id "minis-tab" (:br :class "clear"))
				  (:div :id "dice-tab" 
					(map-markup (list "d3" "d4" "d6" "d8" "d10" "d20" "d100")
						    (:div :class "die-roll-icon" (:span :class "num-dice" "1")
							  elem
							  (:button :class "increment")
							  (:button :class "decrement")))
					(:div :class "coin-flip-icon" "Flip")
					(:br :class "clear")))))
		(:div :id "new-deck-setup" :class "overlay"
		      (:h3 "New Deck")
		      (:div :class "body"
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
				  (:button :class "cancel" "Cancel")))))
	     (when *lobby-stream* 
	       (chain *lobby-stream* (close))
	       (setf *lobby-stream* nil))
	     
	     ($ "#load-form" (change (fn ($upload "#load-form" "/table/load"))))

	     ($ "#zoomed-card button.hide" 
		(button (create :icons (create :primary "ui-icon-zoomout") :text nil))
		(click (fn ($ "#zoomed-card" (hide)))))

	     (show-chat "#game-chat")
	     ($ ".increment" (button (create :icons (create :primary "ui-icon-plus") :text nil)))
	     ($ ".decrement" (button (create :icons (create :primary "ui-icon-minus") :text nil)))

	     ($click ".die-roll-icon .increment"
		     (let ((trg ($ this (siblings ".num-dice"))))
		       ($ trg (text (min 4096 (+ 1 ($int trg))))))

		     "#save-board"
					; TODO 
		     ;;;; I suspect this won't work in the general case. 
		     ;;;; Checks out in Chromium, Firefox and Conkeror though.
		     (setf location (+ "/table/save?table=" *current-table-id*))

		     ".die-roll-icon .decrement"
		     (let ((trg ($ this (siblings ".num-dice"))))
		       ($ trg (text (max 1 (- ($int trg) 1)))))

		     "#custom-deck"
		     ($ "#new-deck-setup" (show))

		     "#new-deck-setup button.add-card"
		     ($append "#new-deck-setup .cards"
			      (:li (:button :class "remove") 
				   (:span :class "content" ($ "#new-deck-setup textarea.new-card" (val))) 
				   (:button :class "add")))

		     "#new-deck-setup button.cancel"
		     ($ "#new-deck-setup" (hide))

		     "#new-deck-setup button.ok"
		     (let ((deck-name ($ "#new-deck-setup .deck-name" (val)))
			   (card-type ($ "#new-deck-setup .card-type" (val))))
		       (setf (aref *local-decks* deck-name)
			     (create 'deck-name deck-name 
				     'card-type card-type 
				     'cards (loop for card-elem in ($ "#new-deck-setup .cards .content")
					       collect (let ((txt ($ card-elem (text))))
							 (try (string->obj txt)
							      (:catch (error) txt))))))
		       (unless ($exists? (+ ".new-deck.new-custom-deck[title=" deck-name "]"))
			 ($prepend "#decks-tab" (:div :class "new-deck new-custom-deck" 
						      :title deck-name deck-name))
			 ($draggable "#decks-tab .new-custom-deck:first" (:revert t)))
		       ($ "#new-deck-setup" (hide))))
	     
	     ($ "#new-deck-setup"
		(on :click "button.remove"
		    (lambda (event) 
		      ($ this (parent) (remove))))
		(on :click "button.add"
		    (lambda (event)
		      (log ($ this (parent) (clone)))
		      ($ "#new-deck-setup .cards" (append ($ this (parent) (clone)))))))

	     ($droppable "#hand" (:overlapping "#board, .stack")
			 (:card (unless ($ dropped (has-class :card-in-hand))
				  (hand/pick-up ($ dropped (attr :id))))))
	     
	     ($draggable ".moveable" (:handle "h3"))
	     ($draggable ".new-deck" (:revert t))
	     ($draggable ".die-roll-icon, .coin-flip-icon" (:revert t :cancel ".increment, .decrement"))
	     ($ "#backpack" (tabs))

	     ($click "#leave" (play/leave-table))
	     
	     (setf *table-stream*
		   (event-source (+ "/ev/" (chain *current-table-id* (to-upper-case)))
				 (joined)
				 (left)
				 (said)
				 (loaded (look-table))
				 (changed-tag)
				 (moved 
				  (with-slots (thing x y) ev
				    ($ (+ "#" thing) (offset (create :left x :top y)))))
				 (took-control (log "Someone took something"))
				 (flipped 
				  ($ (+ "#" (@ ev card id)) (remove))
				  (case (@ ev card type)
				    ("card" (create-card "body" (@ ev card)))
				    ("stack" (create-stack "body" (@ ev card))))
				  (log "Someone flipped something" ev))
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
	       ($droppable board-selector ()
			   (:card-in-hand 
			    (hand/play ($ dropped (attr :id))
				       (if shift? :down :up) (@ event client-x) (@ event client-y) 0 0))
			   
			   (:new-custom-deck
			    ;; play/new-stack-from-json
			    (play/new-stack-from-json
			     (obj->string (aref *local-decks* ($ dropped (text))))
			     (@ event client-x) (@ event client-y) 0 0))

			   (:new-deck
			    (play/new-stack-from-deck ($ dropped (text)) (@ event client-x) (@ event client-y) 0 0))

			   (:die-roll-icon
			    (string->die-roll ($ dropped (text))))
			   (:coin-flip-icon
			    (play/coin-toss)))
	       ($ chat-selector 
		  (empty) 
		  (append ($map (@ table history)
				($ chat-selector (prepend (chat-message elem))))))
	       (scroll-to-bottom chat-selector)
	       ($map ts 
		     (cond ((= (@ elem type) :stack) (create-stack "body" elem))
			   ((= (@ elem type) :card) (create-card "body" elem))))))
	   
	   (defun render-hand (cards)
	     (let ((hand-selector "#hand"))
	       ($ hand-selector (empty))
	       ($map cards (create-card-in-hand hand-selector elem))))

	   (define-thing stack
	       (:div :id (self id) :class "stack" :style (self position)
		     (:button :class "draw" "Draw")
		     (:button :class "shuffle" "Shuffle")
		     (:div :class "card-count" (+ "x" (self card-count))))
	     ($draggable css-id () (play/move (self id) (@ ui offset left) (@ ui offset top) 0 0))
	     ($droppable css-id (:overlapping "#board")
			 (:card-in-hand
			  (hand/play-to ($ dropped (attr :id)) (self id)))
			 (:card
			  (stack/add-to (self id) ($ dropped (attr :id))))
			 (:stack
			  (stack/merge (self id) ($ dropped (attr :id)))))
	     ($click (+ css-id " .draw") (stack/draw (self id) 1)
		     (+ css-id " .shuffle") (stack/shuffle (self id))))

	   (define-thing card 
	       (:div :id (self id) :class "card" :style (self position)
		     (:span :class "content" (card-html self))
		     (:button :class "zoom"))
	     ($ (+ css-id " button.zoom")
		(button (create :icons (create :primary "ui-icon-zoomin") :text nil))
		(click (fn ($ "#zoomed-card" (show))
			   ($ "#zoomed-card .content" (empty) (append (card-html self))))))	     
	     ($draggable css-id () 
			 (play/move (self id) (@ ui offset left) (@ ui offset top) 0 0)
			 (when shift? (play/flip (self id)))))

	   (define-thing card-in-hand
	       (:div :id (self id) :class "card card-in-hand"
		     (:span :class "content" (card-html self))
		     (:button :class "zoom"))
	     ($ (+ css-id " button.zoom")
		(button (create :icons (create :primary "ui-icon-zoomin") :text nil))
		(click (fn ($ "#zoomed-card" (show))
			   ($ "#zoomed-card .content" (empty) (append (card-html self))))))
	     ($draggable css-id (:revert t)))))

(to-file "static/js/util.js"
	 (ps 
	   (defun push-chat-message (message)
	     (let* ((re (new (-reg-exp "\\s+$")))
		    (msg (chain message (replace re ""))))
	       (with-slots (messages current-message) *chat-history*
		 (setf current-message (length messages))
		 (unless (= msg (aref messages (- current-message 1)))
		   (chain messages (push msg))
		   (if (> (length messages) 50)
		       (chain messages (splice 0 1))
		       (incf current-message))))))

	   (defun change-stack-count (stack-id by)
	     (let* ((id (+ "#" stack-id))
		    (ct-class (+ id " .card-count"))
		    (count ($int ct-class 1)))
	       (if (= count (- by))
		   ($ id (remove))
		   ($ ct-class (html (+ "x" (+ count by)))))))

	   (defun chat-command (txt)
	     (let ((re (new (-reg-exp "^(@\\S+)"))))
	       (aif (chain txt (match re))
		    (progn ((aref *chat-commands* (@ it 1))
			    (chain txt (substring (length (@ it 1))) (replace " +" "")))
			   ($ "#chat-input" (val ""))
			   t))))
	   
	   (defvar *chat-commands*
	     (create "@roll"   (lambda (txt) (string->die-roll txt))
		     "@toss"   (lambda (txt) (play/coin-toss))
		     "@rename" (lambda (txt) (rename txt))))

	   (defun string->die-roll (die-string)
	     (let-match die-string "(\\d*)d(\\d+)([-+]\\d+)?"
			((num-dice "1")
			 die-size
			 (modifier "0"))
			(play/roll num-dice die-size modifier)))

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

	   (defun chat-card (card)
	     (log "CARD" card)
	     (who-ps-html
	      (who-ps-html (:div :class "card in-chat" 
				 (:span :class "content" (card-html card))))))	   

	   (defun card-html (card)
	     (log card)
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
				   (:div "Face-Down Tarot"))))

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
					  ;;; TODO actually render the cards here. 
					  ;;; In fact TODO anywhere there's a card being sent, it should be rendered in-line
					  (+ "revealed some cards"))
					 ("flipped"
					  (+ "flipped over " (chat-card (@ msg card))))
					 ("playedFromHand"
					  (+ "played " (chat-card (@ msg card)) " from hand"))
					 ("playedFromStack"
					  (+ "played " (chat-card (@ msg card)) " from " (@ msg stack)))
					 ("playedToStack"
					  (+ "played " (chat-card (@ msg card)) " to stack " (@ msg stack)))
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
	       (when scrl? (scroll-to-bottom selector))))))

(to-file "static/js/deal.js"
	 (ps (defvar *current-table-id* nil)
	     (defvar *current-table-tag* nil)
	     
	     (defvar *handlers-list* nil)
	     (defvar *decks-list* nil)
	     (defvar *local-decks* (create))

	     (defvar *chat-history* 
	       (create messages (new (-array))
		       current-message 0))
	     
	     (defvar *lobby-stream* nil)
	     (defvar *table-stream* nil)
	     (defvar *session* nil)
	     
	     (doc-ready 
	      (show-lobby "body")
	      ($ "body"
		 (on :click "#player-info span.player-tag"
		     (lambda (event)
		       ($ this 
			  (replace-with 
			   (who-ps-html (:input :class "player-tag" :value (@ *session* tag)))))
		       ($keydown "#player-info input.player-tag"
				 <esc> ($ this (replace-with (who-ps-html (:span :class "player-tag" (@ *session* tag)))))
				 <ret> (rename ($ this (val))))
		       ($ "#player-info input.player-tag" (focus))))))
	     
	     ;;; Client-side handler definitions
	     (define-ajax lobby/session ()
	       (setf *session* res)
	       ($ "#player-info .player-id" (text (@ *session* id)))
	       ($ "#player-info .player-tag" (text (@ *session* tag)))
	       (when (@ res current-table)
		 (setf *current-table-id* (@ res current-table :id)
		       *current-table-tag* (@ res current-table :tag))
		 (show-table "body")		  
		 (my-hand)
		 (render-board (@ res current-table))))

	     (define-ajax server-info ()
	       (with-slots (handlers decks public-tables) res
		 (setf *handlers-list* handlers
		       *decks-list* decks
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
	       (lobby/session))
	     
	     (define-ajax rename (new-tag)
	       (log "RENAMING!")
	       (setf (@ *session* tag) new-tag)
	       ($ "#table-toolbar .control-row span.player-tag" (text new-tag))
	       ($ "input.player-tag" (replace-with (who-ps-html (:span :class "player-tag" new-tag)))))
	     
	     (define-ajax lobby/new-table (tag passphrase)
	       (log "STARTING TABLE" res)
	       (setf *current-table-id* (@ res :id)
		     *current-table-tag* (@ res :tag))
	       (show-table "body")
	       (my-hand)
	       (render-board res))

	     (define-ajax lobby/join-table (table passphrase)
	       (log "JOINING TABLE" res)
	       (setf *current-table-id* (@ res :id)
		     *current-table-tag* (@ res :tag))
	       (show-table "body")		  
	       (my-hand)
	       (render-board res))

	     (define-ajax play/leave-table ()
	       (log "LEAVING TABLE")
	       (show-lobby "body"))

	     (define-ajax play/speak (message)
	       ($ "#chat-input" (val "")))

	     (define-ajax lobby/speak (message)
	       ($ "#chat-input" (val "")))
	     
	     (define-ajax look-table ()
	       (log "SHOWING BOARD" res)			  
	       (render-board res))
	     
	     (define-ajax my-hand ()
	       (log "SHOWING HAND" res)
	       (render-hand res))
	     (define-ajax hand/pick-up (card)
	       ($ (+ "#" card) (remove))
	       (render-hand res))
	     (define-ajax stack/draw (stack num)
	       (render-hand res))

	     (define-ajax play/coin-toss ())
	     (define-ajax play/roll (num-dice die-size modifier))
	     (define-ajax play/flip (card))

	     (define-ajax play/new-stack-from-deck (deck-name x y z rot))
	     (define-ajax play/new-stack-from-cards (cards))
	     (define-ajax play/new-stack-from-json (deck x y z rot))
	     (define-ajax stack/merge (stack stack-two))
	     (define-ajax stack/add-to (stack card))
	     (define-ajax stack/shuffle (stack))
	     (define-ajax play/move (thing x y z rot))

	     (define-ajax hand/play (card face x y z rot)
	       ($ (+ "#" card) (remove)))
	     (define-ajax hand/play-to (card stack)
	       ($ (+ "#" card) (remove)))))

;;;;; HTML
(to-file "static/index.html"
	 (html-str
	   (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
		  (:head (:title "Tabletop Prototyping System - Deal")
			 (styles "jquery-ui.css" "main.css")
			 (scripts "jquery-2.0.3.min.js" "jquery-ui-1.10.3.custom.min.js" "util.js" "lobby.js" "table.js" "deal.js"))
		  (:body))))