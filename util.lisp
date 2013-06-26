(in-package #:deal)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; All for the custom define-handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type-exp (arg type)
  "Given a symbol name and a type, returns the expression to read that type from a string"
  (cond
    ((eq type :int) `(parse-integer ,arg))
    ((eq type :string) arg)
    ((eq type :json) `(decode-json-from-string ,arg))
    ((or (eq type :keyword)
	 (eq type :facing))
     `(intern (string-upcase ,arg) :keyword))
    ((eq type :table) 
     (with-gensyms (sym)
       `(let ((,sym (parse-integer ,arg)))
	  (or (gethash ,sym (private-tables *server*)) 
	      (gethash ,sym (public-tables *server*))))))
    (t (error "Invalid type label: '~a'" type))))

(defun lookup-assn (arg type)
  (cond 
    ((or (eq type :table) (eq type :table))
     `(assert ,arg))
    ((eq type :facing)
     `(assert (or (eq ,arg :up) (eq ,arg :down))))
    (t nil)))

(defun type-pieces (args)
  "Takes a list of arguments and returns three values:
- The conversion expressions
- The names (for use as final args)
- The lookup assertions"
  (loop for (name type) in args
     collect (list name (type-exp name type)) into convs
     collect name into as 
     when (lookup-assn name type) collect it into assrs
     finally (return (values convs as assrs))))

(defun find-type (type list)
  (find-if (lambda (e) (eq type (second e))) list))

(defmacro define-handler ((name) (&rest args) &body body)
  "Defines handlers with an eye for self-documentation, DRY and portability"
  (let ((opts `(,name :uri ,(concatenate 'string "/" (string-downcase (symbol-name name))))))
    (if (not args)
	`(define-easy-handler ,opts nil (encode-json (progn ,@body)))
	(multiple-value-bind (type-conversion final-args lookup-assertions) (type-pieces args)
	  `(define-easy-handler ,opts ,final-args
	     (assert (and ,@final-args))
	     (let ,type-conversion
	       ,@lookup-assertions
	       (encode-json (progn ,@body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-table-handler ((name) (&rest args) &body body)
  "Specifically defines handlers that deal with tables. 
Takes care ofadding the appropriate argument/type for define-handler, 
and wraps body in with-lock-held"
  `(define-handler (,name) ((table :table) ,@args) 
     (with-lock-held ((lock table))
       ,@body)))

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