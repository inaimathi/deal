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

;;;;;;;;;; JS-related
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro fn (&body body) `(lambda () ,@body))

(defpsmacro log (&body body)
  (when *debugging*
    `(chain console (log ,@body))))

(defpsmacro $post (uri arg-plist &body body)
  `(chain j-query 
	  (post ,uri (create ,@arg-plist)
		(lambda (data status jqXHR)
		  (let ((res (chain j-query (parse-j-s-o-n (@ jqXHR response-text)))))
		    ,@body)))))

(defpsmacro $map (lst &body body)
  `(chain j-query (map ,lst (lambda (elem i) ,@body))))

(defpsmacro obj->string (thing)
  `(chain -j-s-o-n (stringify ,thing)))