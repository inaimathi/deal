(in-package :deal-ui)

(defun px (num) (format nil "~apx" num))

(defun css-centered-box (width height &optional (position 'absolute))
  `(:width ,(px width) :height ,(px height)
	   :left 50% :top 50% :margin-left ,(px (- (/ width 2))) :margin-top ,(px (- (/ height 2)))
	   :position ,position :z-index 10000 ))

(defun css-box (&optional filled?)
  `(:border "1px solid #ccc" :background-color ,(if filled? "#eee" "#fff") :border-radius 4px))

(defparameter css-card-height 70)
(defparameter css-card-size `(:width 50px :height ,(px css-card-height)))
(defparameter css-display-line '(:height 16px :display inline-block))
(defparameter css-pane '(:height 500px :border "1px solid #ddd" :float left :margin "15px 0px 15px 15px" :padding 10px))
(defparameter css-tight '(:margin 0px :padding 0px))
(defparameter css-sub-window `(,@css-tight :overflow auto))
(defparameter css-header `(:margin 0px :padding 3px :border-radius 3px :background-color "#eee"))

(compile-css "static/css/main.css"
	     `((body ,@css-tight :font-family sans-serif)
	       (.clear :clear both)

	       (.floating-menu :font-size x-small :width 150px :position absolute)
	       
	       (.overlay ,@(css-centered-box 400 200 'fixed) ,@(css-box) :padding 10px :display none)
	       (".overlay h3" ,@css-header)
	       
	       (.moveable ,@(css-box) :position absolute :z-index 10001)
	       (".moveable h3" ,@css-header :cursor move :clear both)
	       (".moveable h2" ,@css-header :font-size small :font-style oblique :clear both :margin "5px 0px 5px 0px")
	       (".moveable .contents" :padding 5px)
	       
	       (.stack ,@css-card-size :position absolute :background-color "#ddd" :border "4px solid #ccc" :cursor move)
	       (".stack .card-count" :font-size x-small :text-align right)
	       
	       (.card ,@css-card-size ,@(css-box) :position absolute :cursor move)
	       (".card .content" :font-size small :font-weight bold)
	       (".card .type" :font-size xx-small :text-align right)
	       (.card-in-hand :position relative :z-index 10000)
	       
	       ("#board" :margin 20px :width 1200px :height 800px :border "1px solid #ccc")
	       
	       ("#player-info" :width 400px :top 8px :left 518px)
	       ("#player-info h3 .player-id" :font-size small :vertical-align top)
	       ("#player-info .card" :float left)
	       ("#player-info .card.in-chat" :position relative :display inline-block :float none :cursor auto)
	       ("#player-info .card.in-chat .type" :margin-top 20px)
	       ("#player-info textarea" :width 100% :height 60px)

	       ("#hand" :height ,(px (+ css-card-height 5)))

	       ("#lobby" :min-width 980px)
	       ("#lobby #left-pane" ,@css-pane :width 570px)
	       ("#lobby #right-pane" ,@css-pane :width 300px)
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
	       ("#chat-history li" ,@(css-box :filled) :padding 3px :margin-bottom 2px)
	       ("#chat-history li span" ,@css-display-line :vertical-align text-top :clear both)
	       ("#chat-history li .time" :font-size xx-small :text-align right :padding 5px :padding-right 10px)
	       ("#chat-history li .player" :font-size x-small :font-style oblique :padding-right 0px)
	       ("#chat-history li .player-tag" :font-style oblique :padding-right 5px)
	       ("#chat-history li .message" :height auto :max-width 400px :word-break break-all :margin-left 5px)
	       ("#chat-controls" :border-top "1px solid #ccc" :padding-top 10px)
	       ("#chat-controls textarea" :width 100% :height 60px :margin-bottom 5px)

	       ("#player-info #chat-history" :height 200px :font-size small)
	       ("#player-info #chat-history .time" :display none)
	       ("#player-info #chat-history li .player" :font-size xx-small)
	       ("#player-info #chat-history li .player-tag" :font-weight bold)
	       ("#player-info #chat-history li .message" :display inline)

	       ("#chat-history li.did" ,@(css-box nil) :border none :padding 3px :margin-bottom 2px)
	       ("#chat-history li.did" :font-style oblique :color "#33f")
	       
	       (".chat-button" :margin "3px 0px 0px 0px" :padding "3px 40px")

	       (".ui-tabs" :margin-top 10px)
	       (".ui-tabs .ui-tabs-nav li a" :font-size small)
	       (".ui-tabs .ui-tabs-panel" :padding 5px)
	       (".new-deck" ,@(css-box :filled) ,@css-card-size :font-size small :float left :margin-right 2px)))