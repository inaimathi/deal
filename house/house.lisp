;; house.lisp
(in-package :house)

;;;;;;;;;; System tables
(defparameter *handlers* (make-hash-table :test 'equal))
(defparameter *channels* (make-hash-table))
(defparameter *sessions* (make-hash-table :test 'equal))

;;;;;;;;;; Class definitions
(defclass buffer ()
  ((contents :accessor contents :initform nil)
   (started :reader started :initform (get-universal-time))))

(defclass session ()
  ((started :reader started :initform (get-universal-time))
   (last-poked :accessor last-poked :initform (get-universal-time))
   (token :reader token :initarg :token)
   (session-values :reader session-values :initform (make-hash-table :test 'equal))))

(defclass request ()
  ((resource :accessor resource :initarg :resource)
   (headers :accessor headers :initarg :headers :initform nil)
   (token :accessor token :initarg :token :initform nil)
   (session :accessor session :initarg :session :initform nil)
   (get-params :accessor get-params :initarg :get-params :initform nil)))

(defclass POST (request) 
  ((post-params :accessor post-params :initarg :post-params :initform nil)))
(defclass GET (request) ())

(defclass response ()
  ((content-type :accessor content-type :initform "text/html" :initarg :content-type)
   (charset :accessor charset :initform "utf-8")
   (response-code :accessor response-code :initform "200 OK")
   (cache-control :accessor cache-control :initform nil)
   (connection :accessor connection :initform nil)
   (expires :accessor expires :initform nil)))

;;;;;;;;;; Function definitions
;;; The basic structure of the server is
; buffering-listen -> parse -> session-lookup -> handle -> channel

;;;;; Buffer/listen-related
(defmethod start ((port integer))
  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t))
	(conns (make-hash-table))
        (buffers (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready in (wait-for-input (cons server (hash-keys conns)) :ready-only t)
		  do (if (typep ready 'stream-server-usocket)
			 (setf (gethash (socket-accept ready) conns) :on)
			 (let ((buf (gethash ready buffers (make-instance 'buffer))))
			   (buffer! (socket-stream ready) buf)
			   (when (starts-with-subseq (list #\newline #\return #\newline #\return)
						     (contents buf))
			     (remhash ready conns)
			     (remhash ready buffers)
			     (handle-request ready (parse buf)))))))
      (loop for c being the hash-keys of conns do (loop while (socket-close c)))
      (setf *channel* nil))))

(defmethod buffer! (stream (buffer buffer))
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eql :eof char))
     do (push char (contents buffer))))

;;;;; Parse-related
(defmethod parse ((buf buffer))
  (let ((lines (split "\\r?\\n" (coerce (reverse (contents buf)) 'string))))
    (destructuring-bind (req-type path http-version) (split " " (first lines))
      (assert (string= http-version "HTTP/1.1"))
      (let ((req-t (intern (string-upcase req-type))))
	(assert (member req-t (list 'get 'post)))
	(let ((req (make-instance req-t :resource path)))
	  (loop 
	     for header in (rest lines) for (name value) = (split ": " header)
	     for n = (deal::->keyword name)
	     if (eq n :cookie) do (loop for val in (split "; " value) for (n v) = (split "=" val)
				     when (string= n "token") do (setf (token req) v))
	     else do (push (cons n value) (headers req)))
	  req)))))

;;;;; Session-related
(let ((prng (ironclad:make-prng :fortuna)))
  (defun new-session-token ()
    (cl-base64:usb8-array-to-base64-string
     (ironclad:random-data 32 prng) :uri t)))

(defun new-session! ()
  (let ((session (make-instance 'session :token (new-session-token))))
    (setf (gethash (token session) *sessions*) session)
    session))

(defun get-session! (token)
  (let ((session (gethash token *sessions*)))
    (setf (last-poked session) (get-universal-time))
    session))

(defmethod lookup (key (session session))
  (gethash key (session-values session)))

(defgeneric (setf lookup) (new-value key session)
  (:documentation "Setter for lookup methods"))

(defmethod (setf lookup) (new-value key (session session))
  (setf (gethash key (session-values session)) new-value))

;;;;; Channel-related
(defmethod subscribe! ((channel symbol) (sock usocket))
  (push sock (gethash channel *channels*)))

(defmethod publish! ((channel symbol) (message string))
  (setf (gethash channel *channels*)
	(loop for sock in (gethash channel *channels*)
	   when (ignore-errors
		  (let ((s (socket-stream sock)))
		    (write-string (cat "data: " message crlf crlf) s)
		    (force-output s)))
	   collect it)))