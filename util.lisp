(in-package #:deal)

(proclaim '(inline sym->keyword))

;;;;; Simple anaphora
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body then-form)
  `(aif ,test-form (progn ,@then-form)))

;;;;; Basic macros
(defmacro set-props (target &rest props)
  (assert (every #'symbolp props))
  (with-gensyms (tgt)
    `(let ((,tgt ,target))
       (setf ,@(loop for p in props
		  collect `(,p ,tgt)
		  collect p)))))

(defmacro hash (&rest k/v-pairs)
  (with-gensyms (hash-table)
    `(let ((,hash-table (make-hash-table)))
       ,@(loop for (k v) on k/v-pairs by #'cddr
	    collect `(setf (gethash ,k ,hash-table) ,v))
       ,hash-table)))

(defmacro obj->hash (instance (&rest extra-values) &rest slots)
  "Turns an object into a hash table, with some Deal-specific idiosyncracies.
Evaluates the slots twice, and evaluates slots before extra-values.
This works where it's used inside of Deal, but probably isn't what you want externally."
  (let ((extra-keys (loop for (k v) on extra-values collect k)))
    `(with-slots ,slots ,instance
       (hash ,@(loop for s in slots for sym = (sym->keyword s)
		  unless (member sym extra-keys)
		  collect (sym->keyword s) and collect s)
	     ,@extra-values))))

;;;;; Basic functions
(defun escape-string (a-string)
  (regex-replace-all 
   "[<>]" a-string
   (lambda (str a b match-start c d e)
     (declare (ignore a b c d e))
     (case (aref str match-start)
       (#\< "&lt;")
       (#\> "&gt;")))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-values (hash-table)
  (loop for val being the hash-values of hash-table collect val))

(defun hash-map (fn hash-table)
  (loop with res = (make-hash-table :size (hash-table-count hash-table))
     for key being the hash-keys of hash-table
     for val being the hash-values of hash-table
     do (setf (gethash key res) (funcall fn val))
     finally (return res)))

(defun pick (a-list)
  "Randomly selects an element from the given list with equal probability."
  (nth (random (length a-list)) a-list))

(defun shuffle (a-list)
  "Returns a randomly sorted copy of the given list"
  (let ((l (copy-seq a-list)))
    (sort l #'> :key (lambda (n) (declare (ignore n)) (random 1.0)))))

(defmethod take ((count integer) (seq list))
  (loop for elem in seq repeat count collect elem))

(defmethod take ((count integer) (seq string))
  (if (> (length seq) count) (subseq seq 0 count) seq))

(defmethod drop ((count integer) (seq list))
  (loop for (elem . rest) on seq repeat (- count 1)
     finally (return rest)))

(defun make-id () (sym->keyword (gensym)))

(defun sym->keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun getj (item json-assoc-list)
  (cdr (assoc item json-assoc-list)))

;;;;;;;;;; Game-related utility
(defun roll (num-dice die-size)
  (loop for d = (+ 1 (random die-size)) repeat num-dice
     collect d into rolls summing d into total
     finally (return (values total rolls))))