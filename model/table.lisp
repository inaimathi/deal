(in-package :deal)

(defclass table ()
  ((id :accessor id :initarg :id)
   (started :reader started :initform (get-universal-time))
   (players :accessor players :initform nil :initarg :players)
   (things :accessor things :initform nil :initarg :things)
   (passphrase :accessor passphrase :initform nil :initarg :passphrase)
   (tablecloth :accessor tablecloth :initform nil :initarg :tablecloth)
   (current-player :accessor current-player :initform nil :initarg :current-player)
   (events :accessor events :initform nil :initarg :events)
   (lock :initform (make-lock) :accessor lock)))

;;;;;;;;;; Game elements
(defclass placeable ()
  ((x :accessor x :initform 0 :initarg :x)
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
  ((cards :accessor cards :initform nil :initarg :cards)
   (card-count :accessor card-count :initform 0 :initarg :card-count)
   (face :initform :down)))

(defclass counter (placeable)
  ((counter-value :accessor counter-value :initarg :counter-value)))

(defclass mini (placeable)
  ((sprite :accessor sprite :initarg :sprite)))

;;;;;;;;;; Publish methods
(defmethod publish ((stack stack))
  (if-up stack stack
	 (cons '(cards) 
	       (remove-if (lambda (pair) (eq (first pair) 'cards)) 
			  (to-alist stack)))))