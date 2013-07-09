;;;; deal-ui.lisp
(in-package #:deal-ui)

;;;;;;;;;; File generation
(compile-css "static/css/main.css"
	     '((body :font-family sans-serif)
	       
	       (.stack :position absolute :width 50px :height 70px :background-color "#ddd" :border "4px solid #ccc")
	       (".stack .card-count" :font-size x-small :text-align right)

	       (.card :width 50px :height 70px :background-color "#fff" :border "2px solid #ccc")
	       (".card .content" :font-size small :font-weight bold)
	       (".card .type" :font-size xx-small :text-align right)
	       
	       (\#hand :width 100% :height 100px :bottom 0px :left 0px :position absolute :padding-left 20px)
	       ("#hand .card" :float left :margin-left 10px)))

(to-file "static/js/render.js"
	 (ps (defvar render 
	       (create
		:stack (lambda (container stack)
			 (log container stack)
			 ($ container 
			    (append (who-ps-html (:div :id (@ stack id) 
						       :class (+ "stack" (when (= (@ stack face) "down") " face-down"))
						       :style (+ "top:" (@ stack y) "px;"
								 "left:" (@ stack x) "px;"
								 "z-index:" (@ stack z) ";"
								 "transform:rotate(" (@ stack rot) "deg)")
						       :title (@ stack id)
						       (:button :class "draw" "Draw")
						       (:div :class "card-count" (+ "x" (@ stack card-count)))))))
			 (let* ((stk (+ "#" (@ stack id))))
			   ($ stk (draggable (create :stop (lambda (event ui) (move (@ stack id) (@ ui offset left) (@ ui offset top) 0 0)))))
			   ($ (+ stk " .draw") (click (fn (draw (@ stack id) 1))))))

		:card (lambda (container card)
			(log container card)
			($ container
			   (append (who-ps-html 
				    (:div :id (@ card id)
					  :class (+ "card" (when (= (@ card face) "down") " face-down"))
					  :title (@ card id)
					  (:span :class "content" (@ card content))
					  (:div :class "type" (@ card card-type)))))))

		:hand (lambda (container hand)
			(loop for card in hand
			   do ($ container (append (who-ps-html (:p card))))))))))

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
	     
	     (defun render-board (table)
	       (let ((board-selector "#board"))
		 ($ board-selector (empty))
		 ($map (@ table things)
		       ((@ render (@ elem type)) board-selector elem))))
	     
	     (defun render-hand (cards)
	       (let ((hand-selector "#hand"))
		 ($ hand-selector (empty))
		 ($map cards 
		       (log elem)
		       ((@ render :card) hand-selector elem))))

	     ;;; Client-side handler definitions
	     (define-ajax show-table "/show-table" ()
			  (log "SHOWING BOARD" res)			  
			  (render-board res))

	     (define-ajax show-hand "/my-hand" ()
			  (log "SHOWING HAND" res)
			  (render-hand res))
	     
	     (define-ajax new-stack "/play/new-stack-from-deck" (deck-name face x y z rot)
			  (setf *table* res)
			  (log "NEW DECK" res))
	     
	     (define-ajax draw "/stack/draw" (stack num)
			  (log "DREW" res)
			  (render-hand res))
	     
	     (define-ajax move "/play/move" (thing x y z rot)
			  (log "MOVED" res))
	     
	     (define-ajax play "/hand/play" (card face x y z rot)
			  (log "PLAYED" res))))

(to-file "static/index.html"
	 (html-str
	   (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
		  (:head (:title "Tabletop Prototyping System - Deal")
			 (styles "main.css")
			 (scripts "jquery.min.js" "jquery-ui.min.js" "render.js" "deal.js"))
		  (:body (:button :id "btn-add-deck" "Add Deck")
			 (:div :id "board")
			 (:div :id "hand")))))