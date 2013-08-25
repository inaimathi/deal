(in-package :deal)

(defun type-expression (arg type)
  "Given a symbol name and a type, returns the expression to read that type from a string"
  (match type
    (:string 
     nil)
    (:int 
     `(parse-integer ,arg :junk-allowed t))
    (:json 
     `(decode-json-from-string ,arg))
    ((or :keyword :facing)
     `(intern (string-upcase ,arg) :keyword))
    (:table 
     (lookup-expression arg '(private-tables *server*) '(public-tables *server*)))
    ((list :list _)
     `(loop for elem in (decode-json-from-string ,arg)
	 collect (gethash elem (things table))))
    ((or :stack :flippable :placeable
	 (list :card :from-table))
     (lookup-expression arg '(things table)))
    ((list :card :from-hand)
     (lookup-expression arg '(hand (session-value :player))))
    (_ (error "Invalid type label: '~a'" type))))

(defun lookup-expression (arg &rest places)
  (with-gensyms (sym)
    `(let ((,sym (intern (string-upcase ,arg) :keyword)))
       (or ,@(loop for p in places
		collect `(gethash ,sym ,p))))))

(defun lookup-assertion (arg type)
  (match type
    (:table `(assert (typep ,arg 'table)))
    (:stack `(assert (typep ,arg 'stack)))
    (:facing `(assert (or (eq ,arg :up) (eq ,arg :down))))
    (:placeable `(assert (typep ,arg 'placeable)))
    (:flippable `(assert (typep ,arg 'flippable)))
    ((list :card _) `(assert (typep ,arg 'card)))
    ((list :list lst-type)
     `(assert (every (lambda (a) (typep a ',lst-type)) ,arg)))
    (_ nil)))

(defun type-pieces (args)
  "Takes a list of arguments and returns five values:
- The final argument list (symbols with no type annotations)
- The table lookup expressions
- The table-related assertion expressions
- The other conversion expressions
- The lookup assertion expressions for all non-table arguments

  Tables and others have to be separaed because the other lookups/assertions
need access to a valid Table object to do anything other than error. Tables
also need to be treated specially by some of the handler-definition macros."
  (loop for (name type) in args for table? = (eq type :table)
     for t-exp = (type-expression name type)
     for assn = (lookup-assertion name type)
     collect name into arguments 
     if table? collect (list name t-exp) into table-lookups and collect assn into table-assertions
     when (and (not table?) t-exp) collect (list name t-exp) into type-expressions
     when (and (not table?) assn) collect it into assertions
     finally (return (values arguments table-lookups table-assertions type-expressions assertions))))

(defmacro with-handler-prelude (&body body)
  `(progn
     (assert (symbolp name) nil "`name` must be a symbol")
     (let* ((uri (concatenate 'string "/" (string-downcase (symbol-name name))))
	    (opts (list name :uri uri)))
       (setf (gethash uri *handlers*) args)
       (multiple-value-bind (final-args table-lookups table-assertions type-conversions lookup-assertions)
	   (type-pieces args)
	 ,@body))))

(defmacro define-handler ((name) (&rest args) &body body)
  "Defines standard handlers (those with no need for complex table interactions, and those with no arguments)"
  (with-handler-prelude
    `(define-easy-handler ,opts ,final-args
       ,@(if (not args)
	     `((encode-json-to-string (progn ,@body)))
	     `((assert (and ,@final-args))
	       (let* ,(append table-lookups type-conversions)
		 ,@(append table-assertions lookup-assertions)
		 (encode-json-to-string (progn ,@body))))))))

(defmacro define-table-handler ((name) (&rest args) &body body)
  "Defines table-specific handlers. These need to establish a lock on the named table BEFORE doing the rest of their assertions/lookups."
  (assert (eq :table (second (first args))) nil "First argument in a table handler must be a table.")
  (with-handler-prelude
    `(define-easy-handler ,opts ,final-args
       (assert (and ,@final-args))
       (let* ,table-lookups
	 ,@table-assertions
	 (with-lock-held ((lock ,(caar table-lookups)))
	   (let* ,type-conversions
	     ,@lookup-assertions
	     (encode-json-to-string (progn ,@body))))))))

(defmacro define-player-handler ((name) (&rest args) &body body)
  "Defines a table-specific handler that checks whether the requester is a player at the named table.
This could be folded in with define-table-handler, if not for lobby/join (which is a handler that both
needs to establish a lock on the named table, AND must deal with players who are not sitting at the named table yet)."
  (assert (eq :table (second (first args))) nil "First argument in a player handler must be a table.")
  (with-handler-prelude
    `(define-easy-handler ,opts ,final-args
       (assert (and ,@final-args))
       (let* ,table-lookups
	 ,@table-assertions
	 (assert (and (session-value :player) (member (session-value :player) (players ,(first (first args))))))
	 (with-lock-held ((lock ,(caar table-lookups)))
	   (let* ,type-conversions
	     ,@lookup-assertions
	     (encode-json-to-string (progn ,@body))))))))