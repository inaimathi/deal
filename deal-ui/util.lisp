(in-package #:deal-ui)

(defmacro with-overwrite (stream file-name &body body)
  (with-gensyms (fname)
    `(let ((,fname ,file-name))
       (ensure-directories-exist ,fname)
       (with-open-file (,stream ,fname :direction :output :if-exists :supersede :if-does-not-exist :create)
	 ,@body))))

(defmacro html (&body body)
  `(with-html-output-to-string (*standard-output*)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
	    ,@body)))

(defmethod to-file ((fname string) dat) (to-file (pathname fname) dat))
(defmethod to-file ((fname pathname) (dat string))
  (with-overwrite stream fname
    (format stream dat)))

(defun scripts (&rest files)
  (with-html-output (*standard-output*)
    (dolist (f files)
      (htm 
       (:script :src (concatenate 'string "/static/js/" f) 
		:type "text/javascript")))))