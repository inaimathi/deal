(in-package #:deal)

;;;;; Simple anaphorics/custom flow control constructs
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body then-form)
  `(aif ,test-form (progn ,@then-form)))

(defmacro if-up (thing up down)
  `(if (eq :up (face ,thing))
       ,up
       ,down))

;;;;; Basic macros
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;;;; Basic functions
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun shuffle (a-list)
  "Returns a randomly sorted copy of the given list"
  (let ((l (copy-seq a-list)))
    (sort l #'> :key (lambda (n) (declare (ignore n)) (random 1.0)))))

(defun take (count a-list)
  (loop for elem in a-list repeat count
     collect elem))

(defun drop (count a-list)
  (loop for (elem . rest) on a-list repeat (- count 1)
     finally (return rest)))

(defun remove-nth (n a-list)
  (remove-if (constantly t) a-list :start n :end (1+ n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; All for the custom define-handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type-exp (arg type)
  "Given a symbol name and a type, returns the expression to read that type from a string"
  (match type
    (:string nil)
    (:int `(parse-integer ,arg))
    (:json `(decode-json-from-string ,arg))
    ((or :keyword :facing)
     `(intern (string-upcase ,arg) :keyword))
    (:table 
     (with-gensyms (sym)
       `(let ((,sym (parse-integer ,arg)))
	  (or (gethash ,sym (private-tables *server*)) 
	      (gethash ,sym (public-tables *server*))))))
    ((or :stack :flippable :placeable)
     (with-gensyms (sym)
       `(let ((,sym (parse-integer ,arg)))
	  (gethash ,sym (things table)))))
    ((list :card :from-table)
     (with-gensyms (sym)
       `(let ((,sym (parse-integer ,arg)))
	  (gethash ,sym (things table)))))
    ((list :card :from-hand)
     (with-gensyms (sym)
       `(let ((,sym (parse-integer ,arg)))
	  (nth ,sym (hand *player*)))))
    (_ (error "Invalid type label: '~a'" type))))

(defun lookup-assn (arg type)
  (match type
    (:table `(assert ,arg))
    (:stack `(assert (typep ,arg 'stack)))
    (:facing `(assert (or (eq ,arg :up) (eq ,arg :down))))
    (:placeable `(assert (typep ,arg 'placeable)))
    (:flippable `(assert (typep ,arg 'flippable)))
    ((list :card _) `(assert (typep ,arg 'card)))
    (_ nil)))

(defun type-pieces (args)
  "Takes a list of arguments and returns three values:
- The conversion expressions
- The names (for use as final args)
- The lookup assertions"
  (loop for (name type) in args
     when (aif (type-exp name type) (list name it)) collect it into convs
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
	     ,(if type-conversion
		  `(let* ,type-conversion
		     ,@lookup-assertions
		     (encode-json ,@body))
		  `(progn ,@lookup-assertions
			  (encode-json ,@body))))))))