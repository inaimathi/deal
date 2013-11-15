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
   (session-token :accessor session-token :initarg :session-token :initform nil)
   (parameters :accessor parameters :initarg :parameters :initform nil)))

(defclass response ()
  ((content-type :accessor content-type :initform "text/html" :initarg :content-type)
   (charset :accessor charset :initform "utf-8")
   (response-code :accessor response-code :initform "200 OK" :initarg :response-code)
   (cookie :accessor cookie :initform nil :initarg :cookie)
   (cache-control :accessor cache-control :initform nil)
   (keep-alive? :accessor keep-alive? :initform nil)
   (expires :accessor expires :initform nil)
   (body :accessor body :initform nil :initarg :body)))

;;;;;;;;;; Function definitions
;;; The basic structure of the server is
; buffering-listen -> parse -> session-lookup -> handle -> channel

;;;;; Buffer/listen-related
(defmethod start ((port integer))
  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t))
	(conns (make-hash-table))
        (buffers (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready in (wait-for-input (cons server (deal::hash-keys conns)) :ready-only t)
		  do (if (typep ready 'stream-server-usocket)
			 (setf (gethash (socket-accept ready) conns) :on)
			 (let ((buf (gethash ready buffers (make-instance 'buffer))))
			   (buffer! (socket-stream ready) buf)
			   (when (starts-with-subseq (list #\newline #\return #\newline #\return)
						     (contents buf))
			     (remhash ready conns)
			     (remhash ready buffers)
			     (handle-request ready (parse buf)))))))
      (loop for c being the deal::hash-keys of conns
	 do (loop while (socket-close c)))
      (loop while (socket-close server)))))

(defmethod buffer! (stream (buffer buffer))
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eql :eof char))
     do (push char (contents buffer))))

;;;;; Parse-related
(defmethod parse ((buf buffer))
  (let ((lines (split "\\r?\\n" (coerce (reverse (contents buf)) 'string))))
    (destructuring-bind (req-type path http-version) (split " " (first lines))
      (declare (ignore req-type))
      (assert (string= http-version "HTTP/1.1"))
      (let* ((path-pieces (split "\\?" path))
	     (resource (first path-pieces))
	     (parameters (second path-pieces))
	     (req (make-instance 'request :resource resource :parameters parameters)))
	(loop 
	   for header in (rest lines) for (name value) = (split ": " header)
	   for n = (deal::->keyword name)
	   do (format t "~a -> ~a :: ~a ~a~%" name value (eq n :cookie) n)
	   if (eq n :cookie) do (setf (session-token req) value)
	   else do (push (cons n value) (headers req)))
	req))))

;;;;; Handling requests
(defmethod handle-request ((sock usocket) (req request))
  (aif (lookup (resource req) *handlers*)
       (let* ((check? (aand (session-token req) (get-session! it)))
	      (sess (aif check? it (new-session!)))
	      (res (funcall it sess (parameters req))))
	 (unless check? (setf (cookie res) (token sess)))
	 (write! res sock)
	 (force-output (socket-stream sock))
	 (socket-close sock))
       (progn
	 (write!
	  (make-instance 
	   'response 
	   :response-code "404 Not Found"
	   :content-type "text/plain"
	   :body "Resource not found...") sock)
	 (socket-close sock))))

(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

(defmethod write! ((res response) (stream stream))
  (flet ((write-ln (&rest strings)
	   (mapc (lambda (str) (write-string str stream)) strings)
	   (crlf stream)))
    (write-ln "HTTP/1.1 " (response-code res))  
    (write-ln "Content-Type: " (content-type res) "; charset=" (charset res))
    (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
    (awhen (cookie res)
      (write-ln "Set-Cookie: " it))
    (when (keep-alive? res) 
      (write-ln "Connection: keep-alive")
      (write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
    (awhen (body res)
      (write-ln "Content-Length: " (write-to-string (length it))) (crlf stream)
      (write-ln it))
    (crlf stream)
    (values)))

(defmethod write! (msg (sock usocket))
  (write! msg (socket-stream sock)))

;;;;; Defining Handlers
(defmacro define-handler (name &body body)
  (let ((uri (format nil "/~(~a~)" name)))
    `(progn
       (awhen (gethash ,uri *handlers*)
	 (warn ,(format nil "Redefining handler '~a'" uri)))
       (setf (gethash ,uri *handlers*)
	     (lambda (session parameters)
	       (declare (ignorable session parameters))
	       (make-instance 'response :body (progn ,@body)))))))

(define-handler index.html
  "<html><head><title>Test Page</title></head><body><h1>Hello from House!</h1></body></html>")

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
  (awhen (gethash token *sessions*)
    (setf (last-poked it) (get-universal-time))
    it))

(defmethod lookup (key (hash hash-table))
  (gethash key hash))

(defmethod lookup (key (session session))
  (gethash key (session-values session)))

(defgeneric (setf lookup) (new-value key session)
  (:documentation "Setter for lookup methods"))

(defmethod (setf lookup) (new-value key (session session))
  (setf (gethash key (session-values session)) new-value))

(defmethod (setf lookup) (new-value key (hash hash-table))
  (setf (gethash key hash) new-value))

;;;;; Channel-related
(defmethod subscribe! ((channel symbol) (sock usocket))
  (push sock (gethash channel *channels*)))

(defmethod publish! ((channel symbol) (message string))
  (setf (gethash channel *channels*)
	(loop for sock in (gethash channel *channels*)
	   when (ignore-errors
		  (let ((s (socket-stream sock)))
		    (write-string "data: " s)
		    (write-string message s)
		    (crlf s) (crlf s)
		    (force-output s)))
	   collect it)))