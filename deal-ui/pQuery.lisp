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

(defpsmacro define-ajax (name uri arg-list &body body)
  `(defun ,name ,arg-list
     (log *current-table-id* ,@arg-list)
     ($post ,uri (:table *current-table-id* ,@(args->plist arg-list))
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
  (deal::with-gensyms (thing container)
    `(defun ,(intern (format nil "create-~a" name)) (container thing)
       (let* ((,thing thing)
	      (,container container)
	      (css-id (+ "#" (@ ,thing id))))
	 ($ ,container (append (who-ps-html ,(expand-self-expression markup thing))))
	 ,@(loop for clause in behavior
	      collect (expand-self-expression clause thing))))))

;; (ps (define-thing card 
;; 	(:div :id (self id)
;; 	      :class (+ "card" (if (= (self face) :down) " face-down" "")) :style (self position)
;; 	      (:span :class "content" (self content))
;; 	      (:div :class "type" (self card-type)))
;;       (draggable (create :stop (lambda (elem ui) (move (self id) (@ ui offset left) (@ ui offset top) 0 0))))))