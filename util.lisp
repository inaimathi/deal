(in-package #:deal)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro define-handler ((name &key (default-type :integer)) (&rest args) &body body)
  (let ((opts `(,name :uri (concatenate 'string "/" (string-downcase (symbol-name name))))))
    (if (not args)
	`(define-easy-handler ,opts nil (encode-json (progn ,@body)))
	(flet ((type-exp (arg type)
		 (case type
		   (:integer `(parse-integer ,arg))
		   (:string arg)
		   (:keyword `(intern (string-upcase ,arg) :keyword)))))
	  (let ((type-conversion (mapcar (lambda (a) 
					   (if (atom a) 
					       (list a (type-exp a default-type))
					       (list (car a) (type-exp (first a) (second a)))))
					 args))
		(final-args (mapcar (lambda (a) (if (atom a) a (car a))) args)))
	    `(define-easy-handler ,opts ,final-args
	       (let ,type-conversion
		 (encode-json (progn ,@body)))))))))

(defmacro define-game-handler ((name &key (default-type :integer)) (&rest args) &body body)
  `(define-handler (,name :default-type ,default-type) (game-id ,@args) ,@body))

(defmacro if-up (thing up down)
  `(if (eq :up (face ,thing))
       ,up
       ,down))

(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun shuffle (a-list)
  "Returns a randomly sorted copy of the given list"
  (let ((l (copy-seq a-list)))
    (sort l #'> :key (lambda (n) (declare (ignore n)) (random 1.0)))))