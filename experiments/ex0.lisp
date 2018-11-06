

(defun string-starts-with (str pre)
    (string-equal (subseq str 0 (length pre)) pre))
(defun string-ends-with (str suf)
    (string-equal (subseq str (- (length str) (length suf))) suf))



(defun read-all-lines (sm rs)
    (let ((line (read-line sm nil nil)))
        (if line (read-all-lines sm (cons line rs)) rs)))
(defun read-markdown-file (filename)
    (with-open-file (sinp filename :direction :input)
        (cons (list :source filename)
              (nreverse (read-all-lines sinp '())))))

