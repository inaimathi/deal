(defpackage :house  
  (:use :cl #:optima #:cl-ppcre #:usocket)
  (:import-from #:alexandria :starts-with-subseq)
  (:import-from #:anaphora :aif :awhen :aand :it))