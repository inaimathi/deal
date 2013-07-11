;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;;;;;;; File generation
;;;;; CSS
(defparameter css-hand-height 100)
(defparameter css-card-size '(:width 50px :height 70px))

(defun css-square (side-length) (list :width (px side-length) :height (px side-length) :border "1px solid #ddd"))

(compile-css "static/css/main.css"
	     `((body :font-family sans-serif)
	       
	       (.stack ,@css-card-size :position absolute :background-color "#ddd" :border "4px solid #ccc")
	       (".stack .card-count" :font-size x-small :text-align right)

	       (.card ,@css-card-size :background-color "#fff" :border "1px solid #ccc" :position absolute)
	       (".card .content" :font-size small :font-weight bold)
	       (".card .type" :font-size xx-small :text-align right)
	       (.card-in-hand :position relative)
	       
	       (\#board ,@(css-square 500) :margin-bottom ,(px (* 1.5 css-hand-height)))

	       (\#hand :width 100% :height ,(px css-hand-height) :bottom 0px :left 0px :position fixed :padding-left 20px :border "1px solid #ddd" :background-color "#fff")
	       ("#hand .card" :float left :margin-left 10px)))

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
				(cond ((= (@ elem type) :stack) (create-stack board-selector elem))
				      ((= (@ elem type) :card) (create-card board-selector elem)))))))
	     
	     (defun render-hand (cards)
	       (let ((hand-selector "#hand"))
		 ($ hand-selector (empty))
		 (when cards
		   ($map cards
			 (create-card-in-hand hand-selector elem)))))

	     (define-thing stack
		 (:div :id (self id) 
		       :class (+ "stack" (when (= (self face) "down") " face-down"))
		       :style (self position)
		       :title (self id)
		       (:button :class "draw" "Draw")
		       (:div :class "card-count" (+ "x" (self card-count))))
	       ($draggable css-id () 
			   (move (self id) (@ ui offset left) (@ ui offset top) 0 0))
	       ($ (+ css-id " .draw") (click (fn (draw (self id) 1)))))
	     
	     (define-thing card 
		 (:div :id (self id)
		       :class (+ "card" (if (= (self face) "down") " face-down" ""))
		       :style (self position)
		       (:span :class "content" (self content))
		       (:div :class "type" (self card-type)))
	       ($draggable css-id () 
			   (move (self id) (@ ui offset left) (@ ui offset top) 0 0)))

	     (define-thing card-in-hand
		 (:div :id (self id)
		       :class (+ "card card-in-hand" (if (= (self face) "down") " face-down" ""))
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
		     (setf *tables-list* res
			   *current-table-id* (@ *tables-list* 0))
		     (show-table)
		     (show-hand))
	      ($ "#btn-add-deck" (click (fn (new-stack (@ *decks-list* 0) :down 0 0 0 0)))))

	     ;;; Client-side handler definitions
	     (define-ajax show-table "/show-table" ()
			  (log "SHOWING BOARD" res)			  
			  (render-board res))

	     (define-ajax show-hand "/my-hand" ()
			  (log "SHOWING HAND" res)
			  (render-hand res))
	     
	     (define-ajax new-stack "/play/new-stack-from-deck" (deck-name face x y z rot)
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
			  (render-board res))))

;;;;; HTML
(to-file "static/index.html"
	 (html-str
	   (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
		  (:head (:title "Tabletop Prototyping System - Deal")
			 (styles "main.css")
			 (scripts "jquery.min.js" "jquery-ui.min.js" "render.js" "deal.js"))
		  (:body (:button :id "btn-add-deck" "Add Deck")
			 (:div :id "board")
			 (:div :id "hand")))))