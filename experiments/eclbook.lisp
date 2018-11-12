;;;;
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;

;;;
;;;
;;;
(defun string-starts-with (str pre)
    (string-equal (subseq str 0 (length pre)) pre))
(defun string-ends-with (str suf)
    (string-equal (subseq str (- (length str) (length suf))) suf))

;;;
;;; Կառուցել markdown օբյեկտները
;;;
(defun create-header (line)
  (let ((p (position-if #'(lambda (c) (char-not-equal c #\#))
                        (string-trim " " line))))
    (let ((sh (length (subseq line 0 p)))
          (ew (string-trim " " (subseq line p))))
      (list (nth (1- sh) '(:h1 :h2 :h3 :h4)) ew))))

(defun create-quoting (line)
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

;;;
;;; Որոշել տողի կատեգորիան
;;;
(defun categorize-line (line)
  (let ((eline (string-left-trim " " line)))
    (cond
      ((zerop (length eline))
       (list :e ""))
      ((char-equal #\# (char eline 0))
       (create-header eline))
      ((char-equal #\> (char eline 0))
       (create-quoting eline))
      ((string-starts-with eline "[^")
       (create-reference eline))
      ((string-ends-with eline "  ")
       (create-verse eline))
      (t (create-paragraph eline)))))
;;;
;;; Կարդալ մեկ markdown ֆայլ
;;;
(defun read-all-lines (sm rs)
  (let ((line (read-line sm nil nil)))
    (if (null line)
        rs
        (read-all-lines sm (cons (categorize-line line) rs)))))
(defun read-markdown-file (filename)
  (let* ((src (pathname filename))
         (dest (make-pathname :defaults src :type "html")))
    (with-open-file (sinp src :direction :input)
      (cons (list :markdown src :html dest)
            (nreverse (read-all-lines sinp '()))))))



;;;
;;; Միավորել նման օբյեկտները
;;;
(defun join-items (lines type joiner)
  (cond
    ((endp lines)
     '())
    ((and (eq type (caar lines)) (eq type (caadr lines)))
     (join-items (funcall joiner lines) type joiner))
    (t (cons (car lines) (join-items (cdr lines) type joiner)))))
(defun join-two-paragraphs (lines)
  (cons (list :p (format nil "~a ~a" (cadar lines) (cadadr lines)))
        (cddr lines)))
(defun join-two-verses (lines)
  (cons (append '(:v) (cdar lines) (cdadr lines))
        (cddr lines)))

(defun remove-empty-lines (lines)
  (delete-if #'(lambda (e) (eq :e (car e))) lines))


;;;
;;; Կառուցել HTML տեքստը
;;;
(defun replace-with (line rpl start end)
  (concatenate 'string (subseq line 0 start) rpl (subseq line end)))

(defun process-one-markup (line be en mf)
  (let ((b (search be line)))
    (if (null b)
        (values nil line)
        (let ((e (search en line :start2 (+ (length be) b))))
          (if (null e)
              (error "Unmatched markup.")
              (let ((ss (funcall mf (subseq line (+ b (length be)) e))))
                (values t (replace-with line ss b (+ e (length en))))))))))

(defun process-markup (line be en mf)
  (multiple-value-bind (cn mln)
      (process-one-markup line be en mf)
    (if cn (process-markup mln be en mf) line)))

(defun process-bold (line)
  (process-markup line "__" "__" #'(lambda (e) (format nil "<b>~a</b>" e))))

(defun process-italic (line)
  (process-markup line "_" "_" #'(lambda (e) (format nil "<i>~a</i>" e))))

(defun process-reference (line)
  (process-markup line "[^" "]" #'(lambda (e) (format nil "<sup><small><a href='~a'>~a</a></small></sup>" e e))))

(defun process-text (line)
  (process-reference (process-italic (process-bold line))))


;;;
;;; create HTML objects
;;;
(defun create-html-header (hdr)
  (let ((id (map 'string #'(lambda (c) (if (char-equal #\Space c) #\- c))
                 (string-downcase (cadr hdr))))
        (ty (string-downcase (symbol-name (car hdr)))))
    (list (car hdr) id (format nil "<~a id='~a'>~a</~a>" ty id (cadr hdr) ty))))

(defun create-html-paragraph (par)
  (cons :p (process-text (cadr par))))

(defun create-html-verse (ver)
  (list :v (format nil "<div style='verse'>~%~{~a<br/>~%~}</div>"
                  (mapcar #'process-text (cdr ver)))))

(defun create-html-quoting (quo)
  (list :q (format nil "<blockquote>~a</blockquote>" (cadr quo))))











;;;
;;; TEST
;;;
(print
 (remove-empty-lines
      (join-items (join-items
        (read-markdown-file "case00.md")
        :p #'join-two-paragraphs) :v #'join-two-verses)
    )
)



(terpri)(quit)

