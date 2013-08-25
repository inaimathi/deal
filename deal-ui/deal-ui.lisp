;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;;;;;;; File generation
;;;;; CSS
(defparameter css-card-size '(:width 50px :height 70px))
(defun px (num) (format nil "~apx" num))

(defparameter css-display-line '(:height 16px :display inline-block))
(defparameter css-pane '(:height 500px :border "1px solid #ddd" :float left :margin "15px 0px 15px 15px" :padding 10px))
(defparameter css-tight '(:margin 0px :padding 0px))
(defparameter css-sub-window `(,@css-tight :overflow auto))
(defparameter css-header `(:margin 0px :padding 3px :border-radius 3px :background-color "#eee"))

(defun css-centered-box (width height &optional (position 'absolute))
  `(:width ,(px width) :height ,(px height)
	   :left 50% :top 50% :margin-left ,(px (- (/ width 2))) :margin-top ,(px (- (/ height 2)))
	   :position ,position :z-index 10000 ))

(defun css-box (&optional filled?)
  `(:border "1px solid #ccc" :background-color ,(if filled? "#eee" "#fff") :border-radius 4px))

(compile-css "static/css/main.css"
	     `((body ,@css-tight :font-family sans-serif)
	       (.clear :clear both)

	       (.floating-menu :font-size x-small :width 150px :position absolute)
	       
	       (.overlay ,@(css-centered-box 400 200 'fixed) ,@(css-box) :padding 10px :display none)
	       (".overlay h3" ,@css-header)
	       
	       (.stack ,@css-card-size :position absolute :background-color "#ddd" :border "4px solid #ccc" :cursor move)
	       (".stack .card-count" :font-size x-small :text-align right)

	       (.card ,@css-card-size ,@(css-box) :position absolute :cursor move)
	       (".card .content" :font-size small :font-weight bold)
	       (".card .type" :font-size xx-small :text-align right)
	       (.card-in-hand :position relative :z-index 10000)
	       
	       ("#board" :margin 20px :width 1200px :height 800px :border "1px solid #ccc")
	       
	       ("#hand-container" ,@(css-box) :width 400px :height 120px :top 8px :left 518px :position absolute :z-index 10001)
	       ("#hand-container h3" ,@css-header :cursor move)
	       ("#hand" :clear both :padding 3px)
	       ("#hand .card" :float left)

	       ("#lobby" :min-width 980px)
	       ("#lobby .left-pane" ,@css-pane :width 570px)
	       ("#lobby .right-pane" ,@css-pane :width 300px)
	       ("#lobby ul" :padding 0px :list-style-type none)
	       ("#lobby ul li" :margin-top 5px)
	       
	       ("#open-tables" ,@css-sub-window :height 400px)
	       ("#open-tables li" ,@(css-box :filled) :padding 5px :margin-bottom 5px)
	       ("#open-tables li span" ,@css-display-line)
	       ("#open-tables li .tag" :width 150px :text-align left)
	       ("#open-tables li .id" :font-size x-small)
	       ("#open-tables li .players" :width 50px :padding-right 5px :text-align right)
	       ("#open-tables button, #new-table" :float right)

	       ("#chat-history" ,@css-sub-window :height 400px :width 100%)
	       ("#chat-history li" ,@(css-box :filled) :padding 3px)
	       ("#chat-history li span" ,@css-display-line :vertical-align text-top :clear both)
	       ("#chat-history li .time" :font-size xx-small :text-align right :padding 5px :padding-right 10px)
	       ("#chat-history li .poster" :font-style oblique :padding-right 10px)
	       ("#chat-history li .message" :height auto :max-width 400px :word-break break-all :margin-left 5px)
	       ("#chat-controls" :border-top "1px solid #ccc" :padding-top 10px)
	       ("#chat-controls .text" :width 100% :height 60px :margin-bottom 5px)
	       ("#chat-controls #send" :margin 0px :padding "3px 40px")))

;;;;; JS
(to-file "static/js/lobby.js"
	 (ps
	   (define-component lobby 
	       (:div :id "lobby"
		     (:div :class "left-pane"
			   (:ul :id "chat-history")
			   (:div :id "chat-controls"
				 (:textarea :id "chat-input" :class "text" :type "text")
				 (:button :id "send" "Send")))
		     (:div :class "right-pane"
			   (:ul :id "open-tables")
			   (:ul (:li (:button :id "new-table" "New Table"))))
		     (:div :id "new-table-setup" :class "overlay"
			   (:h3 "New Table")
			   (:input :class "game-tag")
			   (:button :class "ok" "Ok")))
	     ($click "#send" ($post "/lobby/speak" (:message ($ "#chat-input" (val))) ($ "#chat-input" (val ""))))
	     ($click "#new-table-setup .ok" (new-public-table ($ "#new-table-setup .game-tag" (val))))
	     ($click "#new-table" ($ "#new-table-setup" (show)))
	     ($ "#chat-input"
		(keypress (lambda (event)
			    (when (and (= (@ event which) 13) (not (@ event shift-key)))
			      ($ "#send" (click))))))
	     ($post "/lobby/session" ()
		    (log "MY SESSION" res (@ res current-table))
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
					  (said 
					   (with-slots (player message) ev
					     (chat-message "#chat-history" player message)))
					  (changed-nick 
					   (log "Someone changed nicks" ev))
					  (started-table 
					   (render-table-entry (@ ev message)))
					  (filled-table 
					   ($ (+ "#game-" (@ ev message id)) (remove)))
					  (joined
					   (log "Someone joined something" ev (@ ev message id))
					   (let* ((elem ($ (+ "#game-" (@ ev message id) " .players .count")))
						  (new-count (+ 1 ($int elem))))
					     (log elem new-count ($int (+ "game-" (@ ev message id) " .players .count")))
					     (chain elem (text (+ 1 ($int elem))))))))
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
		(:div :id "hand-container"
		      (:h3 "Hand")
		      (:div :id "hand"))
		(:ul :id "board-menu" :class "floating-menu"
		     (:li (:a :href "javascript: void(0)":id "new-deck" "New Deck"))
		     ;;		     (:li (:a :href "javascript: void(0)" "Add Counter"))
		     ;;		     (:li (:a :href "javascript: void(0)" "Add Mini"))
		     ;;		     (:li (:a :href "javascript: void(0)" "Roll"))
		     ;;		     (:li (:a :href "javascript: void(0)" "Flip Coin"))
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
	     ($click "#cancel" ($ "#board-menu" (hide)))
	     (setf *table-stream*
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
				 (flipped-coin (log "Flipped a coin")))))

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
	     ($draggable css-id () (move (self id) (@ ui offset left) (@ ui offset top) 0 0)))

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

(to-file "static/js/util.js"
	 (ps 
	   (defun scrolled-to-bottom? (selector)
	     (let ((sel ($ selector)))
	       (when (>= (+ (chain sel (scroll-top)) (chain sel (inner-height)))
			 (@ sel 0 scroll-height))
		 t)))

	   (defun scroll-to-bottom (selector)
	     (let ((sel ($ selector)))
	       (chain sel (scroll-top (@ sel 0 scroll-height)))))

	   (defun chat-message (selector player message)
	     (let ((re (new (-reg-exp #\newline :g)))
		   (scrl? (scrolled-to-bottom? selector)))
	       ($append selector
			(:li (:span :class "time" 
				    (new (chain (-date) (to-time-string))))
			     (:span :class "poster" (+ (or player "Anon") ":"))
			     (:div :class "message" (chain message (replace re "<br />")))))
	       (when scrl? (scroll-to-bottom selector))))))

(to-file "static/js/deal.js"
	 (ps (defvar *current-table-id* nil)
	     (defvar *handlers-list* nil)
	     (defvar *decks-list* nil)
	     (defvar *lobby-stream* nil)
	     (defvar *table-stream* nil)

	     (doc-ready (show-lobby "body"))
	     
	     ;;; Client-side handler definitions
	     (define-ajax join-table "/lobby/join-table" (table passphrase)
			  (log "JOINING TABLE" res)
			  (setf *current-table-id* (@ res :id))
			  (show-table "body")		  
			  (show-hand)
			  (render-board res))

	     (define-ajax new-public-table "/lobby/new-public-table" (tag)
			  (log "STARTING TABLE" res)
			  (setf *current-table-id* (@ res :id))
			  (show-table "body")
			  (show-hand)
			  (render-board res))
	     
	     (define-ajax look-table "/look-table" ()
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
			 (scripts "jquery-2.0.3.min.js" "jquery-ui-1.10.3.custom.min.js" "util.js" "lobby.js" "table.js" "deal.js"))
		  (:body))))