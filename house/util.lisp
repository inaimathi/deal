(in-package :house)

(defmethod ->keyword ((thing symbol))
  (intern (symbol-name thing) :keyword))

(defmethod ->keyword ((thing string))
  (intern (string-upcase thing) :keyword))

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