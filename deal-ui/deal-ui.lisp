;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;;;;;;; File generation
;;;;; CSS
(defparameter css-card-size '(:width 50px :height 70px))
(defun css-square (side-length) (list :width (px side-length) :height (px side-length) :border "1px solid #ddd"))

(defparameter css-display-line '(:height 16px :display inline-block))

(compile-css "static/css/main.css"
	     `((body :font-family sans-serif)

	       (.floating-menu :font-size x-small :width 150px :position absolute)
	       
	       (.stack ,@css-card-size :position absolute :background-color "#ddd" :border "4px solid #ccc" :cursor move)
	       (".stack .card-count" :font-size x-small :text-align right)

	       (.card ,@css-card-size :background-color "#fff" :border "1px solid #ccc" :position absolute :cursor move)
	       (".card .content" :font-size small :font-weight bold)
	       (".card .type" :font-size xx-small :text-align right)
	       (.card-in-hand :position relative)
	       
	       (\#board ,@(css-square 500))
	       
	       (\#hand-container :width 400px :height 120px :top 8px :left 518px :position absolute :border "1px solid #ddd" :background-color "#fff")
	       ("#hand-container h3" :margin 0px :padding 3px :background-color "#eee" :cursor move)
	       (\#hand :clear both :padding 3px)
	       ("#hand .card" :float left)

	       ("#lobby .left-pane" :width 670px :float left)
	       ("#lobby .right-pane" :width 300px :float left :margin-left 25px)
	       ("#lobby ul" :padding 0px :list-style-type none)
	       ("#lobby ul li" :margin-top 5px)
	       
	       ("#open-games" :height 400px :overflow auto)
	       ("#open-games li span" ,@css-display-line)
	       ("#open-games li .tag" :width 150px :text-align left)
	       ("#open-games li .id" :font-size x-small)
	       ("#open-games li .players" :width 50px :padding-right 5px :text-align right)
	       ("#open-games button, #new-game" :float right)

	       ("#chat-history" :height 400px :width 650px :overflow auto)
	       ("#chat-history li span" ,@css-display-line :vertical-align text-top :clear both)
	       ("#chat-history li .time" :font-size xx-small :text-align right :padding 5px :padding-right 10px)
	       ("#chat-history li .poster" :font-style oblique :padding-right 10px)
	       ("#chat-history li .message" :height auto :width 400px :word-break break-all)
	       ("#chat-controls .text" :width 450px)))

;;;;; JS
(to-file "static/js/render.js"
	 (ps 
	   (define-component lobby 
	       (:div :id "lobby"
		     (:div :class "left-pane"
			   (:ul :id "chat-history")
			   (:ul :id "chat-controls"
				(:li (:input :id "chat-input" :class "text" :type "text")
				     (:button :id "send" "Send"))))
		     (:div :class "right-pane"
			   (:ul :id "open-games")
			   (:ul (:li (:button :id "new-game" "New Game")))))

	     ($click "#send" 
		     ($post "/lobby/speak" (:message ($ "#chat-input" (val)))
			    ($ "#chat-input" (val ""))))
	     ($post "/server-info" ()
		    (with-slots (handlers decks public-tables) res
		      (setf *handlers-list* handlers
			    *decks-list* decks
			    *tables-list* public-tables)
		      (setf *lobby-stream*
			    (event-source "/ev/lobby"
					  (said 
					   (with-slots (player message) ev
					     ($append "#chat-history"
						      (:li (:span :class "time" 
								  (new (chain (-date) (to-time-string))))
							   (:span :class "poster" (+ (or player "Anon") ":"))
							   (:span :class "message" message)))))
					  (changed-nick (log "Someone changed nicks" ev))
					  (started-game (log "New game started" ev))
					  (filled-game (log "Game is now full" ev))))
		      ($map *tables-list*
			    (with-slots (id tag seated of) elem
			      ($prepend "#open-games"
					(:li (:span :class "tag" tag)
					     (:span :class "id" id) 
					     (:span :class "players" seated "/" of)
					     (:button "Join"))))))))
	   
	   (define-component game
	       (:div
		(:div :id "board")
		(:div :id "hand-container"
		      (:h3 "Hand")
		      (:div :id "hand"))
		(:ul :id "board-menu" :class "floating-menu"
		     (:li (:a :href "javascript: void(0)" "New Deck")
			  (:ul (:li (:a :id "new-deck" :href "javascript: void(0)" "54-card french"))
			       (:li (:a :href "javascript: void(0)" "Some other deck"))))
		     (:li (:a :href "javascript: void(0)" "Add Counter"))
		     (:li (:a :href "javascript: void(0)" "Add Mini"))
		     (:li (:a :href "javascript: void(0)" "Roll"))
		     (:li (:a :href "javascript: void(0)" "Flip Coin"))
		     (:li (:a :href "javascript: void(0)" :id "cancel" "Cancel"))))
	     ($draggable "#hand-container" (:handle "h3"))

	     ;;; Menu definition (plus the markup from the HTML file)
	     ($ "#board-menu" (menu) (hide))
	     ($right-click "#board" 
			   (log "RIGHT CLICKED on #board" event ($ "#board-menu" (position)) :left (@ event client-x) :top (@ event client-y))
			   ($ "#board-menu" (show) (css (create :left (@ event client-x) :top (@ event client-y)))))
	     ($click "#new-deck" 
		     (let* ((position ($ "#board-menu" (position)))
			    (x (@ position left))
			    (y (@ position top)))
		       (log :down x y 0 0)
		       (new-deck (@ *decks-list* 0) :down x y 0 0)
		       ($ "#board-menu" (hide))))
;;	     (join-table (@ *tables-list* 0) "")
	     )
	   
	   (defun render-board (table)
	       (let ((board-selector "#board")
		     (ts (@ table things)))
		 ($ board-selector (empty))
		 ($droppable board-selector
		  (:card-in-hand 
		   (play ($ dropped (attr :id)) :up (@ event client-x) (@ event client-y) 0 0)))
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
	       ($draggable css-id () 
			   (move (self id) (@ ui offset left) (@ ui offset top) 0 0)))

	     (define-thing card-in-stack
		 (:div :id (self id)
		       :class (+ "card card-in-stack" face-class)
		       (:span :class "content" (self content))))

	     (define-thing card-in-hand
		 (:div :id (self id)
		       :class (+ "card card-in-hand" face-class)
		       (:span :class "content" (self content))
		       (:div :class "type" (self card-type)))
	       ($draggable css-id (:revert t)))))

(to-file "static/js/deal.js"
	 (ps (defvar *current-table-id* nil)
	     (defvar *handlers-list* nil)
	     (defvar *decks-list* nil)
	     (defvar *tables-list* nil)
	     (defvar *lobby-stream* nil)
	     (defvar *game-stream* nil)

	     (doc-ready (show-lobby "body"))
	     
	     ;;; Client-side handler definitions
	     (define-ajax join-table "/lobby/join-table" (table passphrase)
			  (log "JOINING TABLE" res)
			  (setf *current-table-id* (@ res :id))
			  (setf *game-stream*
				(event-source (+ "/ev/" (chain *current-table-id* (to-upper-case)))
					      (joined (log "New Player joined"))

					      (moved 
					       (with-slots (thing x y) ev
						 ($ (+ "#" thing) (offset (create :left x :top y)))))
					      (took-control (log "Someone took something"))
					      (flipped (log "Someone flipped something"))

					      (new-deck 
					       (log "Plonked down a new deck" ev)
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
					      (flipped-coin (log "Flipped a coin"))))
			  (show-hand)
			  (render-board res))
	     
	     (define-ajax show-table "/show-table" ()
			  (log "SHOWING BOARD" res)			  
			  (render-board res))
	     
	     (define-ajax show-hand "/my-hand" ()
			  (log "SHOWING HAND" res)
			  (render-hand res))
	     
	     (define-ajax new-deck "/play/new-stack-from-deck" (deck-name face x y z rot))
	     
	     (define-ajax draw "/stack/draw" (stack num)
			  (log "DREW" res "FROM" stack)
			  (render-hand res))
	     
	     (define-ajax move "/play/move" (thing x y z rot)
			  (log "MOVED" res))
	     
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
			 (scripts "jquery-2.0.3.min.js" "jquery-ui-1.10.3.custom.min.js" "render.js" "deal.js"))
		  (:body))))