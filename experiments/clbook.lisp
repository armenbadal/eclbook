
(defvar *contents* '())

(defun read-all-lines (sm rs)
  (let ((line (read-line sm nil nil)))
    (if (null line)
        rs
        (read-all-lines sm (cons line rs)))))

(defun read-markdown-file (filename)
  (with-open-file (sinp filename :direction :input)
    (nreverse (read-all-lines sinp '()))))

(defun create-header (line)
  (let ((p (position-if #'(lambda (c) (char-not-equal c #\#))
                        (string-trim " " line))))
    (let ((sh (length (subseq line 0 p)))
          (ew (string-trim " " (subseq line p))))
      (list (nth (1- sh) '(:h1 :h2 :h3 :h4)) ew))))

(defun create-quote (line)
  (list :quote line))

(defun create-reference (line)
  (list :ref line))

(defun create-paragraph (line)
  (list :p (string-trim "" line)))

(defun paragraph-p (line)
  (eq :p (car line)))

(defun join-first-two-items (lines)
  (let ((one (first lines))
        (two (second lines)))
    (if (eq (car one) (car two))
        (cons (list (car one) (format nil "~a ~a" (cadr one) (cadr two)))
              (cddr lines))
        lines)))
   
(defun join-paragraphs (lines)
  (if (endp lines)
      '()
      (if (and (paragraph-p (car lines)) (paragraph-p (cadr lines)))
          (join-paragraphs (join-first-two-items lines))
          (cons (car lines) (join-paragraphs (cdr lines))))))

(defun categorize-one-line (line)
  (cond
    ((zerop (length line))
     (list :empty ""))
    ((char-equal #\# (char line 0))
     (create-header line))
    ((char-equal #\> (char line 0))
     (create-quote line))
    ((string-equal "[^" (subseq line 0 2))
     (create-reference line))
    (t (create-paragraph line))))

(defun categorize-lines (lines)
  (mapcar #'(lambda (e) (categorize-one-line (string-left-trim " " e)))
          lines))

(defun write-html-stream (content filename)
  nil)





;;;
;;; TEST
;;;
(print
 (delete-if #'(lambda (e) (eq :empty (car e)))
            (join-paragraphs
             (categorize-lines
              (read-markdown-file "case00.md")))))



(terpri)(quit)

