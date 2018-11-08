;;;;
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;

;;;
;;; Կարդալ մեկ markdown ֆայլ։
;;;
(defun read-all-lines (sm rs)
  (let ((line (read-line sm nil nil)))
    (if (null line)
        rs
        (read-all-lines sm (cons line rs)))))
(defun read-markdown-file (filename)
  (let* ((src (pathname filename))
         (dest (make-pathname :defaults src :type "html")))
    (with-open-file (sinp src :direction :input)
      (cons (list :markdown src :html dest)
            (nreverse (read-all-lines sinp '()))))))

;;;
;;;
;;;
(defun string-starts-with (str pre)
    (string-equal (subseq str 0 (length pre)) pre))
(defun string-ends-with (str suf)
    (string-equal (subseq str (- (length str) (length suf))) suf))


(defun create-header (line)
  (let ((p (position-if #'(lambda (c) (char-not-equal c #\#))
                        (string-trim " " line))))
    (let ((sh (length (subseq line 0 p)))
          (ew (string-trim " " (subseq line p))))
      (list (nth (1- sh) '(:h1 :h2 :h3 :h4)) ew))))

(defun create-quote (line)
  (list :q line))

(defun create-verse (line)
  (list :v (string-trim " " line)))

(defun create-reference (line)
  (let ((b (position #\^ line))
        (e (position #\] line)))
    (list :r (subseq line (1+ b) e)
          (string-trim " " (subseq line (+ 2 e))))))

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
     (list :e ""))
    ((char-equal #\# (char line 0))
     (create-header line))
    ((char-equal #\> (char line 0))
     (create-quote line))
    ((string-starts-with line "[^")
     (create-reference line))
    ((string-ends-with line "  ")
     (create-verse line))
    (t (create-paragraph line))))

(defun categorize-lines (content)
  (cons (car content)
        (mapcar #'(lambda (e) (categorize-one-line (string-left-trim " " e)))
                (cdr content))))





;;;
;;; TEST
;;;
(mapc #'print
 ;(delete-if #'(lambda (e) (eq :empty (car e)))
 ;           (join-paragraphs
             (categorize-lines
              (read-markdown-file "case00.md")
              )
 ;            )
 ;           )
 )



(terpri)(quit)

