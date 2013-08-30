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

(defpsmacro aif (test-form true-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,true-form ,else-form)))

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
  (with-ps-gensyms (list)
    `(let ((,list ,lst))
       (when ,list (chain j-query (map ,list (lambda (elem i) ,@body)))))))

(defpsmacro map-markup (lst &body elem-markup)
  `(chain (loop for elem in ,lst
	     collect (who-ps-html ,@elem-markup))
	  (join "")))

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

(defpsmacro $click (&rest target/body-list)
  `(progn ,@(loop for (target body) on target/body-list by #'cddr
	       collect `($ ,target (click (lambda (event) ,body))))))

(defpsmacro $append (target &rest html)
  `($ ,target (append (who-ps-html ,@html))))

(defpsmacro $prepend (target &rest html)
  `($ ,target (prepend (who-ps-html ,@html))))

(defpsmacro $keypress (target &rest key/body-pairs)
  `($ ,target
      (keypress
       (lambda (event)
	 (let ((shift? (@ event shift-key))
	       (alt? (@ event alt-key))
	       (ctrl? (@ event ctrl-key))
	       (meta? (@ event meta-key))
	       (<ret> 13) (<esc> 27) (<space> 32) 
	       (<up> 38) (<down> 40) (<left> 37) (<right> 39))
	   (cond ,@(loop for (key body) on key/body-pairs by #'cddr
		      collect `((= (@ event which) ,(if (stringp key) `(chain ,key (char-code-at 0)) key)) ,body))))))))

;;;;;;;;;; Define client-side handlers
(defpsmacro define-ajax (name arg-list &body body)
  `(defun ,name ,arg-list
     ($post ,(format nil "/~(~a~)" name) (,@(unless (member 'table arg-list) `(:table *current-table-id*)) ,@(args->plist arg-list)) 
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
  (with-ps-gensyms (thing container)
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
  (with-ps-gensyms (stream handlers ev)
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