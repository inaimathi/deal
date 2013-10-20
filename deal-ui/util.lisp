(in-package #:deal-ui)

;;;;;;;;;; Compilation-related
(defmacro with-overwrite (stream file-name &body body)
  (with-gensyms (fname)
    `(let ((,fname ,file-name))
       (ensure-directories-exist ,fname)
       (with-open-file (,stream ,fname :direction :output :if-exists :supersede :if-does-not-exist :create)
	 ,@body))))

(defmethod to-file ((fname string) dat) 
  (to-file (pathname fname) dat))

(defmethod to-file ((fname pathname) (dat string))
  (with-overwrite stream fname
    (format stream dat)))

(defun args->plist (arg-syms)
  (loop for arg in arg-syms
     collect (deal::->keyword arg)
     collect arg))

;;;;;;;;;; HTML-related 
(defmacro html-str (&body body)
  "Shortcut for with-html-output-to-string."
  `(with-html-output-to-string (*standard-output*)
     ,@body))

(defmacro html (&body body)
  "Shortcut for with-html-output."
  `(with-html-output (*standard-output*)
     ,@body))

(defun scripts (&rest files)
  "Shortcut for declaring js includes on the front-end."
  (html (dolist (f files)
	  (htm (:script :type "text/javascript"
			:src (concatenate 'string "/static/js/" f))))))

(defun styles (&rest files)
  "Shortcut for declaring CSS includes on the front-end."
  (html (dolist (f files)
	  (htm (:link :rel "stylesheet" :type "text/css"
		      :href (concatenate 'string "/static/css/" f))))))

;;;;;;;;;; CSS-related
(defun px (num) (format nil "~apx" num))