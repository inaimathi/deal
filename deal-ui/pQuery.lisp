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

(defpsmacro $exists? (selector)
    `(> (@ ($ ,selector) length) 0))

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

(defpsmacro $append (target &rest html)
  `($ ,target (append (who-ps-html ,@html))))

(defpsmacro $prepend (target &rest html)
  `($ ,target (prepend (who-ps-html ,@html))))

;;;;;;;;;; Define client-side handlers
(defpsmacro define-ajax (name uri arg-list &body body)
  `(defun ,name ,arg-list
     ($post ,uri (,@(unless (member 'table arg-list) `(:table *current-table-id*)) ,@(args->plist arg-list)) 
	    ,@body)))

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

(defpsmacro define-component (name markup &body behavior)
  `(defun ,(intern (format nil "show-~a" name)) (container)
     ($ container (empty) (append (who-ps-html ,markup)))
     ,@behavior))

(defpsmacro event-source (uri &body name/body-list)
  (with-gensyms (stream handlers ev)
    `(let ((,stream (new (-event-source ,uri)))
	   (,handlers (create ,@(loop for (name . fn-body) in name/body-list
				   collect `,name 
				   collect `(lambda (ev) 
					      (update-chat "#chat-history" (chat-message ev))
					      ,@fn-body)))))
       (setf (@ ,stream onopen) (lambda (e) (log "Stream OPENED!"))
	     (@ ,stream onerror) (lambda (e) (log "Stream ERRORED!" e))
	     (@ ,stream onmessage)
	     (lambda (e)
	       (let ((,ev (string->obj (@ e data))))
		 ((@ ,handlers (@ ,ev type)) ,ev))))
       ,stream)))