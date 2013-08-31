(in-package :deal)

(defclass table ()
  ((id :reader id :initform (make-id))
   (tag :accessor tag :initform "" :initarg :tag)
   (started :reader started :initform (get-universal-time))
   (max-players :accessor max-players :initform 12 :initarg :max-players)
   (player-count :accessor player-count :initform 0)
   (players :accessor players :initform nil)
   (things :accessor things :initform (make-hash-table))
   (passphrase :accessor passphrase :initform nil :initarg :passphrase)
   (tablecloth :accessor tablecloth :initform nil :initarg :tablecloth)
   (history :accessor history :initform nil)
   (lock :accessor lock :initform (make-lock))))

(defmethod full? ((table table)) 
  (with-slots (player-count max-players) table
    (>= player-count max-players)))

;;;;;;;;;; Game elements
(defclass placeable ()
  ((id :accessor id :initform (make-id))
   (x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   (z :accessor z :initform 0 :initarg :z)
   (rot :accessor rot :initform 0 :initarg :rot)
   (belongs-to :accessor belongs-to :initarg :belongs-to)))

(defclass card (placeable)
  ((content :accessor content :initarg :content)
   (face :accessor face :initform :up :initarg :face)
   (card-type :accessor card-type :initarg :card-type)))

(defclass stack (placeable)
  ((cards :accessor cards :initform nil :initarg :cards)
   (card-count :accessor card-count :initform 0 :initarg :card-count)
   (card-type :accessor card-type :initarg :card-type)))

(defclass counter (placeable)
  ((counter-value :accessor counter-value :initarg :counter-value)))

(defclass mini (placeable)
  ((sprite :accessor sprite :initarg :sprite)))

(defun deck->stack (player a-deck)
  "Takes a deck (a list of card texts) and creates a stack (a pile of cards suitable for placing on a table)"
  (make-instance 'stack
		 :belongs-to (id player) :card-type (first a-deck)
		 :card-count (length (rest a-deck))
		 :cards (shuffle (loop for c in (rest a-deck)
				    collect (make-instance 
					     'card :content c :face :down
					     :card-type (first a-deck) :belongs-to (id player))))))

(defmethod publish! ((table table) action-type &optional move (stream-server *stream-server-uri*))
  (let* ((player (session-value :player))
	 (full-move `((type . ,action-type) 
		      (time . ,(get-universal-time)) 
		      (player . ,(id player))
		      (player-tag . ,(tag player))
		      ,@move)))
    (push full-move (history table))
    (http-request (format nil "~apub?id=~a" stream-server (id table))
		  :method :post :content (encode-json-to-string full-move))))

;;;;;;;;;; delete/insert methods (more in model/server.lisp)
(defmethod delete! ((table table) (thing placeable))
  "Removes a thing from the given table"
  (remhash (id thing) (things table)))

(defmethod insert! ((table table) (card card))
  "Place a new card on the given table. Re-assigns (id card) to maintain secrecy about card properties."
  (setf (id card) (make-id)
	(gethash (id card) (things table)) card))

(defmethod insert! ((table table) (thing placeable))
  "Places a new thing on the given table."
  (setf (gethash (id thing) (things table)) thing))

(defmethod insert! ((stack stack) (card card))
  "Inserts the given card into the given stack."
  (setf (id card) (make-id))
  (incf (card-count stack))
  (push card (cards stack)))

(defmethod pop! ((stack stack))
  "First decrements the card-count, then pops a card from the stack."
  (decf (card-count stack))
  (pop (cards stack)))

(defmethod move! ((card card) from to)
  "Method specifically for naive moves. It happens in at least four places, 
and because of our ID system, delete! must be called before insert!, 
so it made sense to formalize this."
  (delete! from card)
  (insert! to card))

;;;;;;;;;; Redact methods
(defmethod redact ((table table))
  `((type . :table)
    (id . ,(id table))
    (tablecloth . ,(tablecloth table)) 
    (things . ,(redact (things table)))
    (players . ,(mapcar #'redact (players table)))
    (history . ,(take 100 (history table)))))

(defmethod redact ((hash-table hash-table))
  (let ((res (make-hash-table)))
    (loop for k being the hash-keys of hash-table
       do (setf (gethash k res) (redact (gethash k hash-table))))
    res))

(defmethod redact ((stack stack))
  (cons '(type . :stack)
	(remove-if  (lambda (pair) (eq (first pair) 'cards)) 
		    (to-alist stack))))

(defmethod redact ((card card))
  (cons '(type . :card)
	(if-up card (to-alist card)
	       (remove-if (lambda (pair) (eq (first pair) 'content))
			  (to-alist card)))))