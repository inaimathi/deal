(in-package :deal)

(defclass server ()
  ((public-tables :accessor public-tables :initform (make-hash-table))
   (private-tables :accessor private-tables :initform (make-hash-table))
   (decks :accessor decks :initform nil :initarg :decks)
   (players :accessor players :initform (make-hash-table))
   (table-count :accessor table-count :initform 0)
   (max-tables :accessor max-tables :initform 10)
   (player-count :accessor player-count :initform 0)
   (max-players :accessor max-players :initform 50)
   (lock :accessor lock :initform (make-lock))))

(defclass player ()
  ((id :accessor id :initarg :id)
   (hand :accessor hand :initform nil :initarg :hand)
   (events :accessor events :initform nil)))

(defmethod insert! ((server server) (table table))
  "Adds a new table to the server"
  (with-slots (table-count private-tables public-tables) server
    (setf (id table) table-count
	  (gethash table-count
		   (if (passphrase table) 
		       private-tables
		       public-tables)) table)
    (incf table-count)
    table))

(defmethod insert! ((server server) (player player))
  "Adds a new player to the server"
  (with-slots (player-count players) server
    (setf (id player) player-count
	  (gethash player-count players) player)
    (incf player-count)
    player))

(defmethod insert! ((table table) (player player))
  "Sits a new player down at the given table"
  (push player (players table)))