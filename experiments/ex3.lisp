
(defconstant +test-cases-0+
  '("# Title 1"
    "##  Title 2"
    "###Title 3"
    "#### Title 4"))

(defun create-header (line)
  (let ((p (position-if #'(lambda (c) (char-not-equal c #\#))
                        (string-trim " " line))))
    (let ((sh (length (subseq line 0 p)))
          (ew (string-trim " " (subseq line p))))
      (list (nth (1- sh) '(:h1 :h2 :h3 :h4)) ew))))

(defun create-html-header (hdr)
  (let ((id (map 'string #'(lambda (c) (if (char-equal #\Space c) #\- c))
                 (string-downcase (cadr hdr))))
        (ty (string-downcase (symbol-name (car hdr)))))
    (list (car hdr) id (format nil "<~a id='~a'>~a</~a>" ty id (cadr hdr) ty))))

(mapc #'(lambda (c) (print (create-html-header (create-header c)))) +test-cases-0+)

(terpri)(quit)

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
 
