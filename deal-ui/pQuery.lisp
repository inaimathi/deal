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

(defpsmacro let-match (target regex (&rest bindings) &body body)
  (with-ps-gensyms (re match)
    `(let* ((,re (new (-reg-exp ,regex)))
	    (,match (chain ,target (match ,re))))
       (when ,match
	 (let ,(loop for a-bind in bindings for i from 1
		  if (symbolp a-bind) collect `(,a-bind (@ ,match ,i)) 
		  else collect `(,(first a-bind) (or (@ ,match ,i) ,(second a-bind))))
	   ,@body)))))

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
  `(chain (loop for elem in ,lst for i from 0
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

(defpsmacro $droppable (target (&key overlapping) &rest class/action-list)
  `($ ,target
      (droppable 
       (create 
	:drop (lambda (event ui)
		(let ((dropped (@ ui helper context))
		      (shift? (@ event shift-key)))
		  (cond ,@(loop for (class . action) in class/action-list
			     collect `(($ dropped (has-class ,class)) ,@action)))
		  ($ ,overlapping (droppable "enable"))))
	,@(when overlapping
		`(:over (fn ($ ,overlapping (droppable "disable")))
		  :out (fn ($ ,overlapping (droppable "enable")))))))))

(defpsmacro $x (elem ev)
  `(- (+ (@ ,ev client-x) ($ window (scroll-left)))
      (parse-int ($ ,elem (css :margin)))))

(defpsmacro $y (elem ev)
  `(- (+ (@ ,ev client-y) ($ window (scroll-top)))
      (parse-int ($ ,elem (css :margin)))))

(defpsmacro $draggable (target (&key revert handle cancel) &body body)
  `($ ,target (draggable (create :stop (lambda (event ui) 
					 (let ((shift? (@ event shift-key)))
					   ,@body))
				 :start (fn ($ this (css "z-index" 100001) ))
				 ,@(when revert `(:revert ,revert))
				 ,@(when handle `(:handle ,handle))
				 ,@(when cancel `(:cancel ,cancel))))))

(defpsmacro $click (&rest target/body-list)
  `(progn ,@(loop for (target body) on target/body-list by #'cddr
	       collect `($ ,target (click (lambda (event) ,body))))))

(defpsmacro $on (context-selector &rest event/selector/behavior-list)
  `($ ,context-selector
      ,@(loop for (ev sel . behav) in event/selector/behavior-list
	   collect `(on ,ev ,sel (lambda (event) ,@behav)))))

(defpsmacro $button (selector (icon-name &key text? (class "control-button")) &body on-click)
  `($ ,selector
      (button (create :icons (create :primary ,(format nil "ui-icon-~(~a~)" icon-name)) :text ,text?))
      (click (lambda (event) ,@on-click))
      ,@(when class `((add-class ,class)))))

(defpsmacro $append (target &rest html)
  `($ ,target (append (who-ps-html ,@html))))

(defpsmacro $prepend (target &rest html)
  `($ ,target (prepend (who-ps-html ,@html))))

(defpsmacro $replace (target &rest html)
  `($ ,target (append (who-ps-html ,@html))))

(defpsmacro $keydown (target &rest key/body-pairs)
  `($ ,target
      (keydown
       (lambda (event)
	 (let ((shift? (@ event shift-key))
	       (alt? (@ event alt-key))
	       (ctrl? (@ event ctrl-key))
	       (meta? (@ event meta-key))
	       (<ret> 13) (<esc> 27) (<space> 32) 
	       (<up> 38) (<down> 40) (<left> 37) (<right> 39)
	       (key-code (or (@ event key-code) (@ event which))))
	   (cond ,@(loop for (key body) on key/body-pairs by #'cddr
		      collect `((= key-code ,(if (stringp key) `(chain ,key (char-code-at 0)) key)) ,body))))))))

(defpsmacro $upload (target-form uri &rest success)
  (with-ps-gensyms (form-data)
    `(let ((,form-data (new (-form-data (aref ($ ,target-form) 0)))))
       (chain j-query
	      (ajax (create :url ,uri
			    :type "POST"
			    :success (lambda (data status jqXHR)
				       ,@success)
			    :error (lambda (jqXHR status error-thrown)
				     (log "UPLOAD ERRORED" jqXHR status error-thrown))
			    :data ,form-data
			    :cache false
			    "contentType" false
			    "processData" false))))))

(defpsmacro $change (target &rest body)
  (with-ps-gensyms (tgt fn)
    `(let ((,tgt ,target)
	   (,fn (lambda (value) ,@body)))
       ($ ,tgt (keydown (lambda (event) (,fn ($ ,tgt (val))))))
       ($ ,tgt (change (lambda (event) (,fn ($ ,tgt (val)))))))))

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
	   (recur '(+ "top:" (self y) "px;" "left:" (self x) "px;" ;; "z-index:" (self z)
		    ";" "transform:rotate(" (self rot) "deg)")))
	  ((and (eq 'self (car form)) (cdr form))
	   `(@ ,self-elem ,@(cdr form)))
	  ((eq 'self (car form))
	   `(,self-elem))
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
	      (css-id (+ "#" (@ ,thing id))))
	 ($ ,container (append (who-ps-html ,(expand-self-expression markup thing))))
	 ,@(loop for clause in behavior
	      collect (expand-self-expression clause thing))))))

(defpsmacro define-component ((name &key (empty? t)) markup &body behavior)
  `(defun ,(intern (format nil "show-~a" name)) (container)
     ($ container ,@(when empty? `((empty))) (append (who-ps-html ,markup)))
     ,@behavior))

(defpsmacro markup-by-card-type (card &rest card-type/face-up/face-down-list)
  (destructuring-bind (f-up f-down) 
      (loop for (card-type face-up face-down) in card-type/face-up/face-down-list
	 collect `(,card-type (who-ps-html ,face-up)) into up
	 collect `(,card-type (who-ps-html ,face-down)) into down
	 finally (return (list up down)))
    (with-ps-gensyms (crd)
      `(let* ((,crd ,card)
	      (face (@ ,crd face))
	      (type (@ ,crd card-type)))
	 (if (= face "down")
	     (case type
	       ,@f-down
	       (t (who-ps-html (:div (:p "Face Down") (:span :class "type" type)))))
	     (let ((content (@ ,crd content)))
	       (if (stringp content)
		   (who-ps-html (:div :class (+ type " " content) content))
		   (case type
		     ,@f-up
		     (t (who-ps-html (:div :class type (obj->string content))))))))))))

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