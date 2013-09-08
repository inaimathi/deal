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
(defparameter css-header `(:margin 0px :padding 5px :border-radius 3px :background-color "#eee"))
(defparameter css-text-block `(:width 100% :height 60px :margin-bottom 5px))

(compile-css "static/css/main.css"
	     `((body ,@css-tight :font-family sans-serif)
	       (.clear :clear both)

	       (.floating-menu :font-size x-small :width 150px :position absolute)

	       (button.control-button :width 22px :height 22px :float right :opacity .7)
	       ("button.control-button:hover" :opacity 1)
	       (".moveable .control-button, .overlay .control-button" :float none)
	       
	       (.overlay ,@(css-centered-box 400 200 'fixed) ,@(css-box) :display none)
	       (".overlay h3" ,@css-header)
	       (".overlay .contents" :padding 10px)
	       (".overlay .contents .row" :margin-bottom 5px)
	       
	       (.moveable ,@(css-box) :position absolute :z-index 10001 :width 400px)
	       (".moveable h3" ,@css-header :cursor move :clear both)
	       (".moveable h2" ,@css-header :font-size small :font-style oblique :clear both :margin "5px 0px 5px 0px")
	       (".moveable .contents" :padding 10px)
	       
	       (.stack ,@(css-box :filled) ,@css-card-size :font-size small :position absolute :cursor move :border-bottom-width 6px)
	       (".stack .card-count" :font-size x-small :float right :width 100% :text-align right)

	       (.mini :position absolute :cursor move)

	       (.tablecloth ,@(css-box) :width 75px :height 75px :padding 5px :margin-right 3px :font-size medium :font-weight bold :float left)
	       
	       (.card ,@css-card-size ,@(css-box) :position absolute :cursor move)
	       (".card .content" :font-size small :font-weight bold :display block :overflow hidden :height 45px)
	       (".card .type" :font-size xx-small :text-align right)
	       (.card-in-hand :position relative :z-index 10000)

	       ("#player-info .player-id" :font-size x-small :font-style oblique :vertical-align top)
	       ("#player-info .player-tag" :font-weight bold :cursor pointer :min-width 20px :border-bottom "1px solid #eee" :display inline-block)
	       ("#lobby #player-info" :border "1px solid #ddd" :padding 8px :border-radius 5px)
	       
	       ("#board" :margin 20px :width 1200px :height 800px :border "1px solid #ccc" :background-repeat no-repeat)

	       ("#zoomed-card" :width 150px :height 210px :top 10px :left 10px :display none)
	       ("#zoomed-card button" :float right)
	       ("#zoomed-card .content" :padding 10px)

	       ("#table-toolbar" :left 1230px :top 20px)

	       ("#table-toolbar h3 span" :font-size small :vertical-align top)
	       ("#table-toolbar h3 .game-id" :font-size x-small)
	       ("#table-toolbar .control-row" :padding 5px :font-size small)
	       ("#table-toolbar .control-row .label" :padding-left 3px)
	       ("#table-toolbar .control-row button" :margin "0px 10px 5px 0px")
	       ("#table-toolbar .card" :float left)
	       ("#table-toolbar .card.in-chat" :position relative :display inline-block :float none :cursor auto)
	       ("#table-toolbar .card.in-chat .type" :margin-top 20px)
	       ("#table-toolbar textarea" ,@css-text-block)

	       ("#hand" :height ,(px (+ css-card-height 5)))

	       ("#lobby" :min-width 980px)
	       ("#lobby #left-pane" ,@css-pane :width 570px)
	       ("#lobby #right-pane" ,@css-pane :width 300px)
	       ("#lobby ul" :padding 0px :list-style-type none)
	       ("#lobby ul li" :margin-top 5px)
	       
	       ("#open-tables" ,@css-sub-window :height 300px)
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
	       ("#chat-controls textarea" ,@css-text-block)

	       ("#new-deck-setup" ,@(css-centered-box 400 365 'fixed))
	       ("#new-deck-setup textarea" ,@css-text-block)
	       ("#new-deck-setup .cards" :height 100px :overflow auto)

	       (".backpack-mini" :max-height 75px :max-width 75px)

	       ("#table-toolbar #chat-history" :height 200px :font-size small)
	       ("#table-toolbar #chat-history .time" :display none)
	       ("#table-toolbar #chat-history li .player" :font-size xx-small)
	       ("#table-toolbar #chat-history li .player-tag" :font-weight bold)
	       ("#table-toolbar #chat-history li .message" :display inline)

	       ("#chat-history li.did" ,@(css-box nil) :border none :padding 3px :margin-bottom 2px)
	       ("#chat-history li.did" :font-style oblique :color "#33f")
	       
	       (".chat-button" :margin "3px 0px 0px 0px" :padding "3px 40px")

	       (".ui-tabs" :margin-top 10px)
	       (".ui-tabs .ui-tabs-nav li a" :font-size small)
	       (".ui-tabs .ui-tabs-panel" :padding 5px)
	       (".new-deck" ,@(css-box :filled) ,@css-card-size :font-size small :float left :margin-right 2px)

	       (".ui-tabs #dice-tab .die-roll-icon, .coin-flip-icon" :width 55px :height 55px :border "1px solid #ccc" :float left :margin 3px :cursor move :text-align center :border-radius 4px :background-color "#fff" :padding-top 6px)
	       (".ui-tabs #dice-tab .die-roll-icon button" :width 22px :height 22px :margin-left 2px)))

;; { "rank": "ace", "suit": "hearts" }