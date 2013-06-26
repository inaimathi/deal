(in-package #:deal)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun type-exp (arg type)
  "Given a symbol name and a type, returns the expression to read that type from a string"
  (case type
    (:int `(parse-integer ,arg))
    (:string arg)
    (:keyword `(intern (string-upcase ,arg) :keyword))
    (:json `(decode-json-from-string ,arg))
    (t (error "Invalid type label : '~a'" type))))

(defmacro define-handler ((name) (&rest args) &body body)
  (let ((opts `(,name :uri ,(concatenate 'string "/" (string-downcase (symbol-name name))))))
    (if (not args)
	`(define-easy-handler ,opts nil (encode-json (progn ,@body)))
	(let ((type-conversion (loop for (name type) in args collect (list name (type-exp name type))))
	      (final-args (mapcar #'first args)))
	  `(define-easy-handler ,opts ,final-args
	     (assert (and ,@final-args))
	     (let ,type-conversion
	       (encode-json (progn ,@body))))))))

(defmacro define-table-handler ((name) (&rest args) &body body)
  `(define-handler (,name) ((table-id :int) ,@args) 
     (let ((table (or (gethash table-id (private-tables *server*)) (gethash table-id (public-tables *server*)))))
       (assert table)
       (with-lock-held ((lock table))
	 ,@body))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

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