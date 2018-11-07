
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
  (process-markup line "[^" "]" #'(lambda (e) (format nil "<sup><small><a href=''>~a</a></small></sup>" e))))

(defun process-markdown (line)
  (process-reference (process-italic (process-bold line))))



(print (map 'string #'(lambda (c) (if (char-equal #\Space c) #\- c))
            (string-downcase "Սա ՀԱՅԵՐԵՆ տեքստ է։")))


(defvar *a0* "This is a __line to__ test __bold__ markup.")
(print (process-bold *a0*))
                      
(defvar *a1* "This is a __line to__ test _italic_ markup.")
(print (process-italic (process-bold *a1*)))

(terpri)(quit)
  
