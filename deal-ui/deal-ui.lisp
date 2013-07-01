;;;; deal-ui.lisp

(in-package #:deal-ui)

(compile-css "static/css/main.css"
	     '((body :font-family sans-serif)

	       (.stack :position absolute :width 50px :height 70px :background-color "#ddd" :border "4px solid #ccc")
	       (".stack .card-count" :font-size x-small :text-align :right)))

(to-file "static/js/render.js"
	 (ps (defvar render 
	       (create
		:stack (lambda (container stack)
			 (log container stack)
			 ($ container 
			    (append (who-ps-html (:div :class (+ "stack " (@ stack id) (when (= (@ stack face) "down") " face-down"))
						       :style (+ "top:" (@ stack y) "px;"
								 "left:" (@ stack x) "px;"
								 "z-index:" (@ stack z) ";"
								 "transform:rotate(" (@ stack rot) "deg)")
						       :title (@ stack id)
						       (:button :class "draw" "Draw")
						       (:div :class "card-count" (+ "x" (@ stack card-count)))))))
			 ($ (+ "." (@ stack id))
			    (draggable 
			     (create 
			      :stop (lambda (event ui)
				      ($post "/play/move"
					     (:table test-game :thing (@ stack id) :x (@ ui offset left) :y (@ ui offset top) :z 0 :rot 0)))))
			    (children ".draw") (click (fn ($post "/stack/draw" 
								 (:table test-game :stack (@ stack id) :num 1) 
								 (log res))))))))))

(to-file "static/js/global.js"
	 (ps (defvar test-game nil)
	     (defvar games nil)
	     (defvar table nil)
	     (doc-ready
	      ($post "/list-tables" () 
		     (setf games res
			   test-game (@ games 0))
		     ($post "/show-table" 
			    (:table test-game)
			    (setf table res)
			    ($map (@ table things)
				  (chain render (stack "#board" elem)))
			    (log res)))
	      ($ "#btn-add-deck"
		 (click 
		  (fn ($post "/play/new-stack-from-deck" (:table test-game :deck-name "54-Card Standard")
			     (setf table res)
			     (log res))))))))

(to-file "static/index.html"
	 (html-str
	   (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
		  (:head (:title "Tabletop Prototyping System - Deal")
			 (styles "main.css")
			 (scripts "jquery.min.js" "jquery-ui.min.js" "render.js" "global.js"))
		  (:body (:button :id "btn-add-deck" "Add Deck")
			 (:div :id "board")
			 (:div :id "hand")))))
