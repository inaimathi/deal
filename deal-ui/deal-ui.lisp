;;;; deal-ui.lisp

(in-package #:deal-ui)

(to-file "static/js/global.js"
	 (ps (defvar test-game "g1015")
	     (defvar test-stack "g1186")
	     (defvar games nil)
	     (defvar table nil)
	     (doc-ready
	      ($post "/list-games" () 
		     (setf games res)
		     (log res))
	      ($post "/show-table" 
		     (:table test-game)
		     (setf table res)
		     (log res))
	      ($ "#test-btn" (click 
			      (fn
			       ($post "/play/new-stack-from-deck" (:table test-game :deck-name "54-Card Standard")
				      (setf table res)
				      (log res)))))
	      ($ "#draw" (click 
			  (fn ($post "/stack/draw" (:table test-game :stack test-stack :num 1)
				     (log res))))))))

(to-file "static/index.html"
	 (html-str
	   (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
		  (:head (:title "Tabletop Prototyping System - Deal")
			 (scripts "jquery.min.js" "jquery-ui.min.js" "global.js"))
		  (:body (:button :id "test-btn" "Test")
			 (:button :id "draw" "Draw")
			 (:div :id "board")))))
