(in-package :deal)

(defclass table ()
  ((id :reader id :initform (intern (symbol-name (gensym)) :keyword))
   (started :reader started :initform (get-universal-time))
   (players :accessor players :initform nil)
   (things :accessor things :initform (make-hash-table))
   (passphrase :accessor passphrase :initform nil :initarg :passphrase)
   (tablecloth :accessor tablecloth :initform nil :initarg :tablecloth)
   (current-player :accessor current-player :initform nil)
   (events :accessor events :initform nil)
   (lock :accessor lock :initform (make-lock))))

;;;;;;;;;; Game elements
(defclass placeable ()
  ((id :reader id :initform (intern (symbol-name (gensym)) :keyword))
   (x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   (z :accessor z :initform 0 :initarg :z)
   (rot :accessor rot :initform 0 :initarg :rot)
   (belongs-to :accessor belongs-to :initarg :belongs-to)))

(defclass flippable (placeable)
  ((face :accessor face :initform :up :initarg :face)))

(defclass card (flippable)
  ((text :accessor text :initarg :text)
   (image :accessor image :initform nil :initarg :image)))

(defclass stack (flippable)
  ((cards :accessor cards :initform nil)
   (face :initform :down)))

(defclass counter (placeable)
  ((counter-value :accessor counter-value :initarg :counter-value)))

(defclass mini (placeable)
  ((sprite :accessor sprite :initarg :sprite)))

(defmethod delete! ((table table) (thing placeable))
  "Removes a thing from the given table"
  (remhash (id thing) things))

(defmethod insert! ((table table) (thing placeable))
  "Places a new thing on the given table."
  (setf (gethash (id thing) (things table)) thing))

(defmethod insert! ((stack stack) (card card))
  "Inserts the given card into the given stack."
  (push card (cards stack)))

;;;;;;;;;; Publish methods
(defgeneric publish (thing)
  (:documentation "Passes the given thing out to the SSE handler.
A better name for this might be `redact`, because it often doesn't publish full info. 
For instance, when a card or stack is flipped, it won't publish text or image."))

(defmethod publish ((table table))
  (mapcar #'publish (things table)))

(defmethod publish ((stack stack))
  (if-up stack stack
	 (cons '(cards) 
	       (remove-if (lambda (pair) (eq (first pair) 'cards)) 
			  (to-alist stack)))))

(defmethod publish ((card card))
  (if-up card card
	 (remove-if (lambda (pair) (or (eq (first pair) 'text) (eq (first pair) 'image)))
		    (to-alist card))))