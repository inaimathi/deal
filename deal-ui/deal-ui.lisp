;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;; JS
(to-file "static/js/lobby.js"
	 (ps
	   (define-component lobby 
	       (:div :id "lobby"
		     (:div :id "left-pane")
		     (:div :id "right-pane"
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
	     ($click "#new-table-setup .ok" (lobby/new-public-table ($ "#new-table-setup .game-tag" (val)))
		     "#new-table-setup .cancel" ($ "#new-table-setup" (hide))
		     "#new-table" (progn ($ "#new-table-setup" (show))
					 ($ "#new-table-setup .game-tag" (focus))))
	     ($keypress "#new-table-setup .game-tag" 
			<ret> ($ "#new-table-setup .ok" (click))
			<esc> ($ "#new-table-setup .cancel" (click)))
	     (server-info))
	   
	   (defun render-table-entry (tbl-entry)
	     (with-slots (id tag seated of) tbl-entry
	       ($prepend "#open-tables"
			 (:li :id (+ "game-" id) 
			      (:span :class "tag" tag)
			      (:span :class "id" id) 
			      (:span :class "players" (:span :class "count" seated) "/" of)
			      (:button :class "join" "Join")))
	       ($click (+ "#game-" id " .join") (lobby/join-table id ""))))

	   (define-component chat
	       (:div :class "chat"
		     (:ul :id "chat-history")
		     (:div :id "chat-controls"
			   (:textarea :id "chat-input" :type "text")
			   (:button :id "send" :class "chat-button" "Send")))
	     ($keypress "#chat-input"
			<ret> (unless shift?
				(chain event (prevent-default))
				($ "#send" (click)))
			<esc> ($ "#chat-input" (val "")))
	     ($click "#lobby .chat #send"
		     (lobby/speak ($ "#chat-input" (val)))
		     "#player-info #send" 
		     (let ((txt ($ "#chat-input" (val))))
		       (if (chain txt (match "^@"))
			   (chat-command txt)
			   (play/speak txt)))))))

(to-file "static/js/table.js"
	 (ps
	   (define-component table
	       (:div
		(:div :id "board")
		(:div :id "player-info" :class "moveable"
		      (:h3 "Player Info")
		      (:div :class "contents"
			    (:button :id "leave" "Leave Table")
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
					(map-markup *decks-list*
						    (:div :class "new-deck" elem))
					(:br :class "clear"))
				  ;; (:div :id "minis-tab" (:br :class "clear"))
				  (:div :id "dice-tab" 
					(map-markup (list "d3" "d4" "d6" "d8" "d10" "d20" "d100")
						    (:div :class "die-roll-icon" (:span :class "num-dice" "1")
							  elem
							  (:button :class "increment")
							  (:button :class "decrement")))
					(:div :class "coin-flip-icon" "Flip")
					(:br :class "clear"))))))
	     (when *lobby-stream* 
	       (chain *lobby-stream* (close))
	       (setf *lobby-stream* nil))
	     (show-chat "#game-chat")
	     ($ ".increment" (button (create :icons (create :primary "ui-icon-plus") :text nil)))
	     ($ ".decrement" (button (create :icons (create :primary "ui-icon-minus") :text nil)))
	     
	     ($click ".die-roll-icon .increment"
		     (let ((trg ($ this (siblings ".num-dice"))))
		       ($ trg (text (min 4096 (+ 1 ($int trg))))))
		     ".die-roll-icon .decrement"
		     (let ((trg ($ this (siblings ".num-dice"))))
		       ($ trg (text (max 1 (- ($int trg) 1))))))

	     ($droppable "#hand" (:overlapping "#board, .stack")
			 (:card (unless ($ dropped (has-class :card-in-hand))
				  (hand/pick-up ($ dropped (attr :id))))))
	     
	     ($draggable ".moveable" (:handle "h3"))
	     ($draggable ".new-deck" (:revert t))
	     ($draggable ".die-roll-icon, .coin-flip-icon" (:revert t :cancel ".increment, .decrement"))
	     ($ "#player-info h3" (html (+ (who-ps-html (:span :class "player-id" (@ *session* id))) (@ *session* tag))))
	     ($ "#backpack" (tabs))

	     ($click "#leave" (play/leave-table))
	     
	     (setf *table-stream*
		   (event-source (+ "/ev/" (chain *current-table-id* (to-upper-case)))
				 (joined)
				 (left)
				 (said)
				 (moved 
				  (with-slots (thing x y) ev
				    ($ (+ "#" thing) (offset (create :left x :top y)))))
				 (took-control (log "Someone took something"))
				 (flipped 
				  ($ (+ "#" (@ ev thing id)) (remove))
				  (case (@ ev thing type)
				    ("card" (create-card "body" (@ ev thing)))
				    ("stack" (create-stack "body" (@ ev thing))))
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
			   (:new-deck
			    (play/new-stack-from-deck ($ dropped (text)) :up (@ event client-x) (@ event client-y) 0 0))
			   (:die-roll-icon
			    (destructuring-bind (num-dice die-size mod) (parse-die-roll ($ dropped (text)))
			      (play/roll num-dice die-size mod)))
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
	       (:div :id (self id) 
		     :class (+ "stack" face-class)
		     :style (self position)
		     (:button :class "draw" "Draw")
		     (:div :class "card-count" (+ "x" (self card-count))))
	     ($draggable css-id () (play/move (self id) (@ ui offset left) (@ ui offset top) 0 0))
	     ($droppable css-id (:overlapping "#board")
			 (:card-in-hand
			  (hand/play-to ($ dropped (attr :id)) (self id)))
			 (:card
			  (stack/add-to (self id) ($ dropped (attr :id))))
			 (:stack
			  (stack/merge (self id) ($ dropped (attr :id)))))
	     ($click (+ css-id " .draw") (stack/draw (self id) 1)))

	   (define-thing card 
	       (:div :id (self id)
		     :class (+ "card" face-class)
		     :style (self position)
		     (:span :class "content" (self content))
		     (:div :class "type" (self card-type)))
	     ($draggable css-id () 
			 (play/move (self id) (@ ui offset left) (@ ui offset top) 0 0)
			 (when shift? (play/flip (self id)))
))

	   (define-thing card-in-hand
	       (:div :id (self id)
		     :class (+ "card card-in-hand" face-class)
		     (:span :class "content" (self content))
		     (:div :class "type" (self card-type)))
	     ($draggable css-id (:revert t)))))

(to-file "static/js/util.js"
	 (ps 
	   (defun change-stack-count (stack-id by)
	     (let* ((id (+ "#" stack-id))
		    (ct-class (+ id " .card-count"))
		    (count ($int ct-class 1)))
	       (if (= count (- by))
		   ($ id (remove))
		   ($ ct-class (html (+ "x" (+ count by)))))))
	   
	   (defun chat-command (txt)
	     (aif (parse-die-command txt)
		  (progn 
		    (destructuring-bind (num-dice die-size modifier) it
		      (play/roll num-dice die-size modifier)
		      ($ "#chat-input" (val "")))
		    t)
		  (if (parse-coin-toss txt)
		      (progn (play/coin-toss)
			     ($ "#chat-input" (val ""))
			     t)
		      nil)))

	   (defun parse-coin-toss (txt)
	     (chain txt (match "^@(flip|toss)")))
	   
	   (defun parse-die-command (txt)
	     (let* ((re (new (-reg-exp "^@roll *(\\d*)d(\\d+)([-+]\\d+)?")))
		    (match (chain txt (match re))))
	       (when match
		 (list (or (parse-int (@ match 1)) 1)
		       (parse-int (@ match 2))
		       (or (parse-int (@ match 3)) 0)))))

	   (defun parse-die-roll (txt)
	     (let* ((re (new (-reg-exp "(\\d*)d(\\d+)([-+]\\d+)?")))
		    (match (chain txt (match re))))
	       (when match
		 (list (or (parse-int (@ match 1)) 1)
		       (parse-int (@ match 2))
		       (or (parse-int (@ match 3)) 0)))))

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
	     (who-ps-html
	      (who-ps-html (:div :class (+ "card in-chat" 
					   (if (= (@ card face) "down") 
					       "face-down" ""))
				 (:span :class "content" (@ card content))
				 (:div :class "type" (@ card card-type))))))

	   (defun chat-message (msg)
	     (with-slots (player player-tag time type message) msg
	       (who-ps-html (:li :class (if (= type "said") "said" "did")
				 (:span :class "time" (new (chain (-date time) (to-time-string))))
				 (:span :class "player" player)
				 (:span :class "player-tag" player-tag)
				 (:div :class "message"
				       (case type
					 ("joined" 
					  "joined the table")
					 ("left" 
					  "left the table")
					 ("said" 
					  (newline->break message))
					 ("moved"
					  (+ "moved " (@ msg thing)))
					 ("changedNick"
					  (+ "changed their tag from " (@ msg old-tag)))
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
					  (+ "flipped over " (chat-card (@ msg thing))))
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
	     (defvar *handlers-list* nil)
	     (defvar *decks-list* nil)
	     (defvar *lobby-stream* nil)
	     (defvar *table-stream* nil)
	     (defvar *session* nil)

	     (doc-ready (show-lobby "body"))
	     
	     ;;; Client-side handler definitions
	     (define-ajax lobby/session ()
	       (setf *session* res)
	       (when (@ res current-table)
		 (setf *current-table-id* (@ res current-table :id))
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
				     (changed-nick)
				     (started-table
				      (render-table-entry (@ ev message)))
				     (filled-table
				      ($ (+ "#game-" (@ ev message id)) (remove)))
				     (joined
				      (let* ((elem ($ (+ "#game-" (@ ev message id) " .players .count")))
					     (new-count (+ 1 ($int elem))))
					(chain elem (text (+ 1 ($int elem))))))
				     (left
				      (let ((sel (+ "#game-" (@ ev message id))))
					(if ($exists? sel)
					    ($ sel (replace (render-table-entry (@ ev message))))
					    (render-table-entry (@ ev message)))))))
		 ($map public-tables
		       (with-slots (seated of) elem
			 (when (< seated of) (render-table-entry elem)))))
	       (lobby/session))
	     
	     (define-ajax lobby/new-public-table (tag)
	       (log "STARTING TABLE" res)
	       (setf *current-table-id* (@ res :id))
	       (show-table "body")
	       (my-hand)
	       (render-board res))

	     (define-ajax lobby/join-table (table passphrase)
	       (log "JOINING TABLE" res)
	       (setf *current-table-id* (@ res :id))
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
	     (define-ajax stack/merge (stack stack-two))
	     (define-ajax hand/pick-up (card)
	       ($ (+ "#" card) (remove))
	       (render-hand res))
	     (define-ajax play/coin-toss ())
	     (define-ajax play/roll (num-dice die-size modifier))
	     (define-ajax play/flip (thing))
	     (define-ajax play/new-stack-from-deck (deck-name face x y z rot))
	     (define-ajax play/new-stack-from-cards (cards)
	       (log "NEW STACK" cards)
	       (render-board res))
	     (define-ajax stack/add-to (stack card))
	     (define-ajax stack/draw (stack num)
	       (render-hand res))
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