;; house.lisp
(in-package :house)

;;;;;;;;;; System tables
(defparameter *handlers* (make-hash-table :test 'equal))
(defparameter *channels* (make-hash-table))
(defparameter *sessions* (make-hash-table :test 'equal))

;;;;;;;;;; Function definitions
;;; The basic structure of the server is
; buffering-listen -> parse -> session-lookup -> handle -> channel

;;;;; Buffer/listen-related
(defmethod start ((port integer))
  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t))
	(conns (make-hash-table))
        (buffers (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
		  do (if (typep ready 'stream-server-usocket)
			 (setf (gethash (socket-accept ready) conns) :on)
			 (let ((buf (gethash ready buffers (make-instance 'buffer))))
			   (buffer! (socket-stream ready) buf)
			   (let ((complete? (complete? buf))
				 (big? (too-big? buf))
				 (old? (too-old? buf)))
			     (when (or complete? big? old?)
			       (remhash ready conns)
			       (remhash ready buffers)
			       (cond (big? (error! +413+ ready))
				     (old? (error! +400+ ready))
				     (t (handler-case
					    (handle-request ready (parse buf))
					  ((not simple-error) () (error! +400+ ready)))))))))))
      (loop for c being the hash-keys of conns
	 do (loop while (socket-close c)))
      (loop while (socket-close server)))))

(defmethod complete? ((buffer buffer))
  (starts-with-subseq (list #\newline #\return #\newline #\return)
		      (contents buffer)))

(defmethod too-big? ((buffer buffer))
  (> (content-size buffer) +max-request-size+))

(defmethod too-old? ((buffer buffer))
  (> (- (get-universal-time) (started buffer)) +max-request-size+))

(defmethod buffer! (stream (buffer buffer))
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eql :eof char))
     do (push char (contents buffer)) do (incf (content-size buffer))))

;;;;; Parse-related
(defmethod parse ((str string))
  (let ((lines (split "\\r?\\n" str)))
    (destructuring-bind (req-type path http-version) (split " " (first lines))
      (declare (ignore req-type))
      (assert (string= http-version "HTTP/1.1"))
      (let* ((path-pieces (split "\\?" path))
	     (resource (first path-pieces))
	     (parameters (second path-pieces))
	     (req (make-instance 'request :resource resource :parameters parameters)))
	(loop 
	   for header in (rest lines) for (name value) = (split ": " header)
	   for n = (->keyword name)
	   if (eq n :cookie) do (setf (session-token req) value)
	   else do (push (cons n value) (headers req)))
	req))))

(defmethod parse ((buf buffer))
  (parse (coerce (reverse (contents buf)) 'string)))

;;;;; Handling requests
(defmethod handle-request ((sock usocket) (req request))
  (aif (lookup (resource req) *handlers*)
       (handler-case
	   (let* ((check? (aand (session-token req) (get-session! it)))
		  (sess (aif check? it (new-session!))))
	     (funcall it sock check? sess (parameters req)))
	 ((not simple-error) () (error! +400+ sock)))
       (error! +404+ sock)))

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

(defmethod write! ((res sse) (stream stream))
  (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
	  (id res) (event res) (retry res) (data res)))

(defmethod write! (msg (sock usocket))
  (write! msg (socket-stream sock)))

(defmethod error! ((err response) (sock usocket))
  (ignore-errors 
    (write! err sock))
  (socket-close sock))

;;;;; Defining Handlers
(defmacro make-closing-handler ((&key (content-type "text/html")) &body body)
  (with-gensyms (cookie?)
    `(lambda (sock ,cookie? session parameters)
       (declare (ignorable session parameters))
       (let ((res (make-instance 
		   'response 
		   :content-type ,content-type 
		   :cookie (unless ,cookie? (token session))
		   :body (progn ,@body))))
	 (write! res sock)
	 (socket-close sock)))))

(defmacro make-stream-handler (&body body)
  (with-gensyms (cookie?)
    `(lambda (sock ,cookie? session parameters)
       (declare (ignorable session parameters))
       (let ((res (progn ,@body)))
	 (write! (make-instance 'response
				:keep-alive? t :content-type "text/event-stream" 
				:cookie (unless ,cookie? (token session))) sock)
	 (awhen res (write! (make-instance 'sse :data it) sock))
	 (force-output (socket-stream sock))))))

(defmacro bind-handler (name handler)
  (let ((uri (format nil "/~(~a~)" name)))
    `(progn
       (when (gethash ,uri *handlers*)
	 (warn ,(format nil "Redefining handler '~a'" uri)))
       (setf (gethash ,uri *handlers*) ,handler))))

(defmacro define-closing-handler ((name &key (content-type "text/html")) &body body)
  `(bind-handler ,name (make-closing-handler (:content-type ,content-type) ,@body)))

(defmacro define-stream-handler ((name) &body body)
  `(bind-handler ,name (make-stream-handler ,@body)))

;;;;; Session-related
(defun new-session-token ()
  (cl-base64:usb8-array-to-base64-string
   (with-open-file (s "/dev/urandom" :element-type '(unsigned-byte 8))
     (make-array 32 :initial-contents (loop repeat 32 collect (read-byte s))))
   :uri t))

(defun new-session! ()
  (let ((session (make-instance 'session :token (new-session-token))))
    (setf (gethash (token session) *sessions*) session)
    session))

(defun get-session! (token)
  (awhen (gethash token *sessions*)
    (setf (last-poked it) (get-universal-time))
    it))

;;;;; Channel-related
(defmethod subscribe! ((channel symbol) (sock usocket))
  (push sock (lookup channel *channels*))
  nil)

(defmethod publish! ((channel symbol) (message string))
  (setf (lookup channel *channels*)
	(loop with msg = (make-instance 'sse :data message)
	   for sock in (lookup channel *channels*)
	   when (ignore-errors 
		  (write! msg sock)
		  (force-output (socket-stream sock))
		  sock)
	   collect it)))

;;;;;;;;;; Test Handlers
(define-closing-handler (interface)
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html><head><title>Test page</title></head><body><div id='console'></div><script type='text/javascript'>var src = new EventSource('/test-stream');
function p(msg) {
    var elem = document.getElementById('console');
    return elem.innerHTML = elem.innerHTML + '<p>' + msg + '</p>';
};
src.onerror = function (e) {
    p('ERROR OCCURRED...');
    return p(JSON.stringify(e));
};
src.onopen = function (e) {
    return p('STREAM OPENED...');
};
src.onmessage = function (e) {
    p('GOT MESSAGE!');
    return p('data: ' + e.data);
};</script></body></html>")

(define-closing-handler (test.json :content-type "application/json")
  "{\"this is\": \"a test\"}")

(define-closing-handler (send-message)
  (publish! :test-channel "You've been pinged, motherfuckers!")
  "Sent message...")

(define-stream-handler (test-stream)
  (subscribe! :test-channel sock))