
(defvar *a0* '(:v "Line 0" "Line 1" "Line 2" "Line 3"))

(defun create-html-verse (ver)
  (list :v (format nil "<div style='verse'>~%~{~a<br/>~%~}</div>" (cdr ver))))

(print (create-html-verse *a0*))

(terpri)(quit)
