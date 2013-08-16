;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;;;;;;; File generation
;;;;; CSS
(defparameter css-card-size '(:width 50px :height 70px))

(defun css-square (side-length) (list :width (px side-length) :height (px side-length) :border "1px solid #ddd"))

(compile-css "static/css/main.css"
	     `((body :font-family sans-serif)

	       (.floating-menu :font-size x-small :width 150px :position absolute)
	       
	       (.stack ,@css-card-size :position absolute :background-color "#ddd" :border "4px solid #ccc" :cursor move)
	       (".stack .card-count" :font-size x-small :text-align right)

	       (.card ,@css-card-size :background-color "#fff" :border "1px solid #ccc" :position absolute :cursor move)
	       (".card .content" :font-size small :font-weight bold)
	       (".card .type" :font-size xx-small :text-align right)
	       (.card-in-hand :position relative)
	       (".stack:hover .cards" :padding-top 50px)
	       (".stack:hover .card" :position relative :margin-top -50px)
	       (".stack:hover .card:hover" :margin-bottom 40px)
	       
	       (\#board ,@(css-square 500))
	       
	       (\#hand-container :width 400px :height 120px :top 8px :left 518px :position absolute :border "1px solid #ddd" :background-color "#fff")
	       ("#hand-container h3" :margin 0px :padding 3px :background-color "#eee" :cursor move)
	       (\#hand :clear both :padding 3px)
	       ("#hand .card" :float left)))

;;;;; JS
(to-file "static/js/render.js"
	 (ps (defun render-board (table)
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
		       (:div :class "cards")
		       (:div :class "card-count" (+ "x" (self card-count))))
	       (when (self cards)
		 ($map (self cards)
		       (create-card-in-stack (+ css-id " .cards") elem)))
	       ($draggable css-id () 
			   (move (self id) (@ ui offset left) (@ ui offset top) 0 0))
	       ($ (+ css-id " .draw") (click (fn (draw (self id) 1)))))
	     
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

	     (doc-ready
	      ($post "/list-handlers" () (setf *handlers-list* res))
	      ($post "/list-decks" () (setf *decks-list* res))
	      ($post "/list-tables" () 
		     (join-table (@ res 0) ""))
	      ($draggable "#hand-container" (:handle "h3"))

	      ;;; Menu definition (plus the markup from the HTML file)
	      ($ "#board-menu" (menu) (hide))
	      ($right-click "#board" 
			    (log "RIGHT CLICKED on #board" event ($ "#board-menu" (position)) :left (@ event client-x) :top (@ event client-y))
			    ($ "#board-menu" (show) (css (create :left (@ event client-x) :top (@ event client-y)))))
	      ($ "#new-deck" (click 
			      (fn 
			       (let* ((position ($ "#board-menu" (position)))
				      (x (@ position left))
				      (y (@ position top)))
				 (log :down x y 0 0)
				 (new-deck (@ *decks-list* 0) :down x y 0 0)
				 ($ "#board-menu" (hide)))))))
	     
	     ;;; Client-side handler definitions
	     (define-ajax join-table "/game/join-table" (table passphrase)
			  (log "JOINING TABLE" res)
			  (setf *current-table-id* (@ res :id))
			  (show-hand)
			  (render-board res))

	     (define-ajax show-table "/show-table" ()
			  (log "SHOWING BOARD" res)			  
			  (render-board res))
	     
	     (define-ajax show-hand "/my-hand" ()
			  (log "SHOWING HAND" res)
			  (render-hand res))
	     
	     (define-ajax new-deck "/play/new-stack-from-deck" (deck-name face x y z rot)
			  (log "NEW DECK" res)
			  (render-board res))
	     
	     (define-ajax draw "/stack/draw" (stack num)
			  (log "DREW" res)
			  (render-hand res))
	     
	     (define-ajax move "/play/move" (thing x y z rot)
			  (log "MOVED" res))
	     
	     (define-ajax play "/hand/play" (card face x y z rot)
			  (log "PLAYED" res)
			  ($ (+ "#" card) (remove))
			  (render-board res))
	     
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
		  (:body (:div :id "board")
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
			      (:li (:a :href "javascript: void(0)" :id "cancel" "Cancel")))))))