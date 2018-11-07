
(defconstant +test-cases-0+
  '("# Վերնագիր 1"
    "##   Վերնագիր 2"
    "###Վերնագիր 3"
    "#### Վերնագիր 4"))

(defun create-header (line)
  (let ((p (position-if #'(lambda (c) (char-not-equal c #\#))
                        (string-trim " " line))))
    (let ((sh (length (subseq line 0 p)))
          (ew (string-trim " " (subseq line p))))
      (list (nth (1- sh) '(:h1 :h2 :h3 :h4)) ew))))

;;;(mapc #'(lambda (c) (print (create-header c))) +test-cases-0+)


(defconstant +test-cases-1+
  '("[^1]: տեքստ տեքստ տեքստ։"
    "[^abc]: տեքստ տեքստ տեքստ։"))

(defun create-reference (line)
  (let ((b (position #\^ line))
        (e (position #\] line)))
    (list :ref (subseq line (1+ b) e)
          (string-trim " " (subseq line (+ 2 e))))))

;;;(mapc #'(lambda (c) (print (create-reference c))) +test-cases-1+)


(defun has-reference (line)
  nil)



(terpri)(quit)    
 
