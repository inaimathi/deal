(in-package #:deal-ui)

(defparameter *debugging* t)

(defpsmacro log (&body body)
  (when *debugging*
    `(chain console (log ,@body))))

;;;;;;;;;; JS Basics
(defpsmacro obj->string (thing)
  `(chain -j-s-o-n (stringify ,thing)))

(defpsmacro string->obj (thing)
  `(chain j-query (parse-j-s-o-n ,thing)))

(defpsmacro fn (&body body) `(lambda () ,@body))

;;;;;;;;;; jQuery Basics
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro $int (selector &optional (start 0))
  `(parse-int (chain ($ ,selector (text)) (substring ,start))))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro $map (lst &body body)
  `(chain j-query (map ,lst (lambda (elem i) ,@body))))

(defpsmacro $post (uri arg-plist &body body)
  `(chain j-query 
	  (post ,uri (create ,@arg-plist)
		(lambda (data status jqXHR)
		  (let ((res (string->obj (@ jqXHR response-text))))
		    ,@body)))))

(defpsmacro $highlight (target)
  `($ ,target (stop t t) (effect :highlight nil 500)))

(defpsmacro $droppable (target &rest class/action-list)
  `($ ,target (droppable 
	       (create 
		:drop (lambda (event ui)
			(let ((dropped (@ ui helper context)))
			  ;; not sure if this should be a cond or a list of independent whens
			  (cond ,@(loop for (class action) in class/action-list
				     collect `(($ dropped (has-class ,class)) ,action)))))))))

(defpsmacro $draggable (target (&key revert handle cancel) &body body)
  `($ ,target (draggable (create :stop (lambda (event ui) ,@body)
				 ,@(when revert `(:revert ,revert))
				 ,@(when handle `(:handle ,handle))
				 ,@(when cancel `(:cancel ,cancel))))))

(defpsmacro $right-click (target &rest body)
  (with-gensyms (fn)
    `(let ((,fn (lambda (event) ,@body)))
       ($ ,target
	  (bind :contextmenu
		(lambda (event)
		  (,fn event)
		  (chain event (prevent-default))))
	  (bind :oncontextmenu
		(lambda (event)
		  (,fn event)
		  (setf (@ window event return-value) false)))))))

(defpsmacro $click (target &rest body)
  `($ ,target (click (lambda (event) ,@body))))

;;;;;;;;;; Menus

;;;;; From
;; ($context-menu ($right-click "#board")
;;;;;          ("New Deck" (sequence (lambda (deck-name) (new-deck deck-name current-x current-y 0 0)) *deck-lists*))
;;;;; 	       ("New Deck" (list ("54-card french" (new-deck (@ *decks-list* 0) :down current-x current-y 0 0))
;; 				 ("Some other deck")))
;; 	       ("Add Counter")
;; 	       ("Add Mini")
;; 	       ("Roll")
;; 	       ("Flip Coin"))

;;;;; To
;; ($ "#board-menu" (menu) (hide))
;; ($right-click "#board" 
;; 	      (log "RIGHT CLICKED on #board" event ($ "#board-menu" (position)) :left (@ event client-x) :top (@ event client-y))
;; 	      ($ "#board-menu" (show) (css (create :left (@ event client-x) :top (@ event client-y)))))
;; ($ "#new-deck" (click 
;; 		(fn 
;; 		 (let* ((position ($ "#board-menu" (position)))
;; 			(x (@ position left))
;; 			(y (@ position top)))
;; 		   (new-deck (@ *decks-list* 0) :down x y 0 0)
;; 		   ($ "#board-menu" (hide))))))
;; (:ul :id "board-menu" :class "floating-menu"
;; 			      (:li (:a :href "javascript: void(0)" "New Deck")
;; 				   (:ul (:li (:a :id "new-deck" :href "javascript: void(0)" "54-card french"))
;; 					(:li (:a :href "javascript: void(0)" "Some other deck"))))
;; 			      (:li (:a :href "javascript: void(0)" "Add Counter"))
;; 			      (:li (:a :href "javascript: void(0)" "Add Mini"))
;; 			      (:li (:a :href "javascript: void(0)" "Roll"))
;; 			      (:li (:a :href "javascript: void(0)" "Flip Coin"))
;; 			      (:li (:a :href "javascript: void(0)" :id "cancel" "Cancel")))

;;;;;;;;;; Define client-side handlers
(defpsmacro define-ajax (name uri arg-list &body body)
  `(defun ,name ,arg-list
     ($post ,uri (,@(unless (member 'table arg-list) `(:table *current-table-id*)) ,@(args->plist arg-list)) 
	    ,@body)))

(defpsmacro define-stream-handlers (name args &body type/body-list)
  (declare (ignore args)) ;; just for indenting purposes
  `(defvar ,name
     (create ,@(loop for (type . fn-body) in type/body-list
		  collect `,type collect `(lambda (ev) ,@fn-body)))))

;;;;;;;;;; Defining markup/behavior hybrids made easier
(defun expand-self-expression (form self-elem)
  (flet ((recur (frm) (expand-self-expression frm self-elem)))
    (cond ((null form) nil)
	  ((atom form) form)
	  ((and (eq 'self (car form)) (eq (second form) 'position))
	   (recur '(+ "top:" (self y) "px;" "left:" (self x) "px;" "z-index:" (self z) ";" "transform:rotate(" (self rot) "deg)")))
	  ((eq 'self (car form)) 
	   `(@ ,self-elem ,@(cdr form)))
	  ((atom (car form)) 
	   (cons (car form) (recur (cdr form))))
	  ((listp (car form)) 
	   (cons (recur (car form))
		 (recur (cdr form)))))))

(defpsmacro define-thing (name markup &body behavior)
  (with-gensyms (thing container)
    `(defun ,(intern (format nil "create-~a" name)) (container thing)
       (let* ((,thing thing)
	      (,container container)
	      (face-class (if (= (@ ,thing face) "down") " face-down" ""))
	      (css-id (+ "#" (@ ,thing id))))
	 ($ ,container (append (who-ps-html ,(expand-self-expression markup thing))))
	 ,@(loop for clause in behavior
	      collect (expand-self-expression clause thing))))))