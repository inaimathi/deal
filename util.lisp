(in-package #:deal)

(proclaim '(inline sym->keyword))

;;;;; Simple anaphors/custom flow control constructs
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

(defmacro set-props (target &rest props)
  (assert (every #'symbolp props))
  (with-gensyms (tgt)
    `(let ((,tgt ,target))
       (setf ,@(loop for p in props
		  collect `(,p ,tgt)
		  collect p)))))

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
  (loop for key being the hash-keys of hash-table
     collect (funcall fn key (gethash key hash-table))))

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

(defun trivial-range (min max)
  (loop for i from min to max collect i))

(defmethod drop ((count integer) (seq list))
  (loop for (elem . rest) on seq repeat (- count 1)
     finally (return rest)))

(defun make-id () (sym->keyword (gensym)))

(defun sym->keyword (symbol)
  (intern (symbol-name symbol) :keyword))

;;;;;;;;;; Game-related utility
(defun roll (num-dice die-size)
  (loop for d = (+ 1 (random die-size)) repeat num-dice
     collect d into rolls summing d into total
     finally (return (values total rolls))))