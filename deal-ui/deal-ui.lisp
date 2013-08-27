;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;; JS
(to-file "static/js/lobby.js"
	 (ps
	   (define-component lobby 
	       (:div :id "lobby"
		     (:div :class "left-pane"
			   (:ul :id "chat-history")
			   (:div :id "chat-controls"
				 (:textarea :id "chat-input" :type "text")
				 (:button :id "send" :class "chat-button" "Send")))
		     (:div :class "right-pane"
			   (:ul :id "open-tables")
			   (:ul (:li (:button :id "new-table" "New Table"))))
		     (:div :id "new-table-setup" :class "overlay"
			   (:h3 "New Table")
			   (:input :class "game-tag")
			   (:button :class "ok" "Ok")
			   (:button :class "cancel" "Cancel")))
	     ($click "#send" (lobby/speak ($ "#chat-input" (val))))
	     ($click "#new-table-setup .ok" (new-public-table ($ "#new-table-setup .game-tag" (val))))
	     ($click "#new-table-setup .cancel" ($ "#new-table-setup" (hide)))
	     ($click "#new-table" ($ "#new-table-setup" (show)))
	     ($ "#chat-input"
		(keypress (lambda (event)
			    (when (and (= (@ event which) 13) (not (@ event shift-key)))
			      (chain event (prevent-default))
			      ($ "#send" (click))))))
	     ($post "/lobby/session" ()
		    (when (@ res current-table)
		      (setf *current-table-id* (@ res current-table :id))
		      (show-table "body")		  
		      (show-hand)
		      (render-board (@ res current-table))))
	     ($post "/server-info" ()
		    (with-slots (handlers decks public-tables) res
		      (setf *handlers-list* handlers
			    *decks-list* decks)
		      (setf *lobby-stream*
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
			      (when (< seated of) (render-table-entry elem)))))))
	   
	   (defun render-table-entry (tbl-entry)
	     (with-slots (id tag seated of) tbl-entry
	       ($prepend "#open-tables"
			 (:li :id (+ "game-" id) 
			      (:span :class "tag" tag)
			      (:span :class "id" id) 
			      (:span :class "players" (:span :class "count" seated) "/" of)
			      (:button :class "join" "Join")))
	       ($click (+ "#game-" id " .join") (join-table id ""))))))

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
			    (:ul :id "chat-history" :class "short")
			    (:textarea :id "chat-input" :type "text")
			    (:button :id "send" :class "chat-button" "Send")))
		(:ul :id "board-menu" :class "floating-menu"
		     (:li (:a :href "javascript: void(0)" :id "new-deck" "New Deck"))
		     (:li (:a :href "javascript: void(0)" :id "cancel" "Cancel"))))
	     ($draggable ".moveable" (:handle "h3"))
	     ($ "#chat-input"
		(keypress (lambda (event)
			    (when (and (= (@ event which) 13) (not (@ event shift-key)))
			      (chain event (prevent-default))
			      ($ "#send" (click))))))
	     ($click "#send" 
		     (let ((txt ($ "#chat-input" (val))))
		       (or (chat-command txt)
			   (table/speak txt))))
	     ($click "#leave" (leave-table))

	     ;;; Menu definition (plus the markup from the HTML file)
	     ($ "#board-menu" (menu) (hide))
	     ($right-click "#board" 
			   ($ "#board-menu" (show) (css (create :left (@ event client-x) :top (@ event client-y)))))

	     ($click "#new-deck" 
		     (let* ((position ($ "#board-menu" (position)))
			    (x (@ position left))
			    (y (@ position top)))
		       (log :down x y 0 0)
		       (new-deck (@ *decks-list* 0) :down x y 0 0)
		       ($ "#board-menu" (hide))))
	     ($click "#cancel" ($ "#board-menu" (hide)))
	     (setf *table-stream*
		   (event-source (+ "/ev/" (chain *current-table-id* (to-upper-case)))
				 (joined)
				 (said)
				 (moved 
				  (with-slots (thing x y) ev
				    ($ (+ "#" thing) (offset (create :left x :top y)))))
				 (took-control (log "Someone took something"))
				 (flipped (log "Someone flipped something"))
				 (new-deck 
				  (create-stack "body" (@ ev stack)))
				 (stacked-up (log "Made a stack from cards"))
				 (merged-stacks (log "Put some stacks together"))
				 (added-to-stack (log "Put a card onto a stack"))
				 (drew-from 
				  (let* ((id (+ "#" (@ ev stack)))
					 (count ($int (+ id " .card-count") 1)))
				    ($ (+ id " .card-count") (html (+ "x" (- count 1)))))
				  ($highlight (+ "#" (@ ev stack))))

				 (peeked (log "Peeked at cards from a stack"))
				 (revealed (log "Showed everyone cards from a stack"))

				 (played-from-hand 
				  (create-card "body" (@ ev card)))

				 (played-from-stack (log "Played the top card from a stack"))
				 (played-to-stack (log "Played to the top of a stack"))
				 (picked-up (log "Picked up a card"))

				 (rolled (log "Rolled"))
				 (flipped-coin (log "Flipped a coin")))))

	   (defun render-board (table)
	     (let ((board-selector "#board")
		   (chat-selector "#chat-history")
		   (ts (@ table things)))
	       ($ board-selector (empty))
	       ($droppable board-selector
			   (:card-in-hand 
			    (play ($ dropped (attr :id)) :up (@ event client-x) (@ event client-y) 0 0)))
	       ($ chat-selector 
		  (empty) 
		  (append ($map (@ table history)
				($ chat-selector (prepend (chat-message elem))))))
	       (scroll-to-bottom chat-selector)
	       (when ts ($map ts 
			      (cond ((= (@ elem type) :stack) (create-stack "body" elem))
				    ((= (@ elem type) :card) (create-card "body" elem)))))))
	   
	   (defun render-hand (cards)
	     (let ((hand-selector "#hand"))
	       ($ hand-selector (empty))
	       (when cards
		 ($map cards
		       (create-card-in-hand hand-selector elem)))))

	   (define-thing stack
	       (:div :id (self id) 
		     :class (+ "stack" face-class)
		     :style (self position)
		     (:button :class "draw" "Draw")
		     (:div :class "card-count" (+ "x" (self card-count))))
	     ($draggable css-id () (move (self id) (@ ui offset left) (@ ui offset top) 0 0))
	     ($click (+ css-id " .draw") (draw (self id) 1)))

	   (define-thing card 
	       (:div :id (self id)
		     :class (+ "card" face-class)
		     :style (self position)
		     (:span :class "content" (self content))
		     (:div :class "type" (self card-type)))
	     ($draggable css-id () (move (self id) (@ ui offset left) (@ ui offset top) 0 0)))

	   (define-thing card-in-hand
	       (:div :id (self id)
		     :class (+ "card card-in-hand" face-class)
		     (:span :class "content" (self content))
		     (:div :class "type" (self card-type)))
	     ($draggable css-id (:revert t)))))

(to-file "static/js/util.js"
	 (ps 
	   (defun chat-command (txt)
	     (aif (parse-die-roll txt)
		  (progn 
		    (destructuring-bind (num-dice die-size modifier) it
		      (roll-dice num-dice die-size modifier)
		      ($ "#chat-input" (val "")))
		    t)
		  (if (parse-coin-toss txt)
		      (progn (flip-coin)
			     ($ "#chat-input" (val ""))
			     t)
		      nil)))

	   (defun parse-coin-toss (txt)
	     (chain txt (match "@(flip|toss)")))

	   (defun parse-die-roll (txt)
	     (let* ((re (new (-reg-exp "@roll *(\\d*)d(\\d+)([-+]\\d+)?")))
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
					 ("playedFromHand"
					  (+ "played " (chat-card (@ msg card)) " from hand"))
					 ("playedFromStack"
					  (+ "played " (chat-card (@ msg card)) " from " (@ msg stack)))
					 ("playedToStack"
					  (+ "played " (chat-card (@ msg card)) " to stack " (@ msg stack)))
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

	     (doc-ready (show-lobby "body"))
	     
	     ;;; Client-side handler definitions
	     (define-ajax new-public-table "/lobby/new-public-table" (tag)
			  (log "STARTING TABLE" res)
			  (setf *current-table-id* (@ res :id))
			  (show-table "body")
			  (show-hand)
			  (render-board res))

	     (define-ajax join-table "/lobby/join-table" (table passphrase)
			  (log "JOINING TABLE" res)
			  (setf *current-table-id* (@ res :id))
			  (show-table "body")		  
			  (show-hand)
			  (render-board res))

	     (define-ajax leave-table "/play/leave" ()
			  (log "LEAVING TABLE")
			  (show-lobby "body"))

	     (define-ajax table/speak "/play/speak" (message)
			  ($ "#chat-input" (val "")))

	     (define-ajax lobby/speak "/lobby/speak" (message)
			  ($ "#chat-input" (val "")))
	     
	     (define-ajax look-table "/look-table" ()
			  (log "SHOWING BOARD" res)			  
			  (render-board res))
	     
	     (define-ajax show-hand "/my-hand" ()
			  (log "SHOWING HAND" res)
			  (render-hand res))
	     
	     (define-ajax flip-coin "/play/coin-toss" ())

	     (define-ajax roll-dice "/play/roll" (num-dice die-size modifier))

	     (define-ajax new-deck "/play/new-stack-from-deck" (deck-name face x y z rot))
	     
	     (define-ajax draw "/stack/draw" (stack num)
			  (log "DREW" res "FROM" stack)
			  (render-hand res))
	     
	     (define-ajax move "/play/move" (thing x y z rot))
	     
	     (define-ajax play "/hand/play" (card face x y z rot)
			  ($ (+ "#" card) (remove)))
	     
	     (define-ajax new-stack-from-cards "/play/new-stack-from-cards" (cards)
			  (log "NEW STACK" cards)
			  (render-board res))))

;;;;; HTML
(to-file "static/index.html"
	 (html-str
	   (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
		  (:head (:title "Tabletop Prototyping System - Deal")
			 (styles "jquery-ui-1.10.3.custom.min.css" "main.css")
			 (scripts "jquery-2.0.3.min.js" "jquery-ui-1.10.3.custom.min.js" "util.js" "lobby.js" "table.js" "deal.js"))
		  (:body))))