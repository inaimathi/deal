(in-package :deal)

(defun type-expression (arg type)
  "Given a symbol name and a type, returns the expression to read that type from a string"
  (match type
    ((or :string (cons :string _)) 
     nil)
    ((or :int (cons :int _)) 
     `(parse-integer ,arg :junk-allowed t))
    (:json 
     `(decode-json-from-string ,arg))
    (:json-file
     `(decode-json-from-source (first ,arg)))
    ((or :keyword :facing)
     `(->keyword ,arg))
    (:table 
     (lookup-expression arg '(private-tables *server*) '(public-tables *server*)))
    ((list :list :keyword)
     `(loop for elem in (decode-json-from-string ,arg)
	 collect (intern (string-upcase elem) :keyword)))
    ((list :list :card)
     `(loop for elem in (decode-json-from-string ,arg)
	 collect (gethash elem (things table))))
    ((or :stack :flippable :placeable :note
	 (list :card :from-table))
     (lookup-expression arg '(things table)))
    ((list :card :from-hand)
     (lookup-expression arg '(hand (lookup :player session))))
    (_ (error "Invalid type label: '~a'" type))))

(defun lookup-expression (arg &rest places)
  (with-gensyms (sym)
    `(let ((,sym (intern (string-upcase ,arg) :keyword)))
       (or ,@(loop for p in places
		collect `(gethash ,sym ,p))))))

(defun lookup-assertion (arg type)
  (match type
    (:table `(assert-http (typep ,arg 'table)))
    (:stack `(assert-http (typep ,arg 'stack)))
    (:facing `(assert-http (member ,arg (list :up :down))))
    (:note `(assert-http (typep ,arg 'note)))
    (:placeable `(assert-http (typep ,arg 'placeable)))
    (:flippable `(assert-http (typep ,arg 'flippable)))
    ((list :string :min min) 
     `(assert-http (>= (length ,arg) ,min)))
    ((list :string :max max)
     `(assert-http (>= ,max (length ,arg))))
    ((list :string :min min :max max)
     `(assert-http (>= ,max (length ,arg) ,min)))
    ((list :int :min min)
     `(assert-http (>= ,arg ,min)))
    ((list :int :max max)
     `(assert-http (>= ,arg ,max)))
    ((list :int :min min :max max)
     `(assert-http (>= ,max ,arg ,min)))
    ((list :card _) `(assert-http (typep ,arg 'card)))
    ((list :list :card)
     `(assert-http (every (lambda (a) (typep a 'card)) ,arg)))
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

(defmacro define-handler (name (&rest args) &body body)
  "Defines standard handlers (those with no need for complex table interactions, and those with no arguments)"
  (multiple-value-bind (final-args table-lookups table-assertions type-conversions lookup-assertions)
      (type-pieces args)
    `(define-closing-handler (,name :content-type "application/json") ,final-args
       ,(if (not args)
	    `(encode-json-to-string (progn ,@body))
	    `(progn
	       (assert-http (and ,@final-args))
	       (let* ,(append table-lookups type-conversions)
		 ,@(append table-assertions lookup-assertions)
		 (encode-json-to-string (progn ,@body))))))))

(defmacro define-table-handler (name (&rest args) &body body)
  "Defines table-specific handlers. These need to establish a lock on the named table BEFORE doing the rest of their assertions/lookups."
  (assert (eq :table (second (first args))) nil "First argument in a table handler must be a table.")
  (multiple-value-bind (final-args table-lookups table-assertions type-conversions lookup-assertions)
      (type-pieces args)
    `(define-closing-handler (,name :content-type "application/json") ,final-args
       (assert (and ,@final-args))
       (let* ,table-lookups
	 ,@table-assertions
	 (with-lock-held ((lock ,(caar table-lookups)))
	   (let* ,type-conversions
	     ,@lookup-assertions
	     (encode-json-to-string (progn ,@body))))))))

(defmacro define-player-handler (name (&rest args) &body body)
  "Defines a table-specific handler that checks whether the requester is a player at the named table.
This could be folded in with define-table-handler, if not for lobby/join (which is a handler that both
needs to establish a lock on the named table, AND must deal with players who are not sitting at the named table yet)."
  (assert (eq :table (second (first args))) nil "First argument in a table handler must be a table.")
  (multiple-value-bind (final-args table-lookups table-assertions type-conversions lookup-assertions)
      (type-pieces args)
    `(define-closing-handler (,name :content-type "application/json") ,final-args
       (assert-http (lookup :player session))
       (assert-http (and ,@final-args))
       (let* ,table-lookups
	 ,@table-assertions
	 (assert-http (member (lookup :player session) (players ,(first (first args)))))
	 (with-lock-held ((lock ,(caar table-lookups)))
	   (let* ,type-conversions
	     ,@lookup-assertions
	     (encode-json-to-string (progn ,@body))))))))
