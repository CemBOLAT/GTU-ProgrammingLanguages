(defun read-file (fileName)
    (with-open-file (stream fileName :direction :input)
        (loop for line = (read-line stream nil)
            while line
            collect line
        )
    )
)

(defun line-by-index (lines index)
    (nth index lines)
)

(defun write-file (lines)
    (with-open-file (stream "output.lisp" :direction :output :if-exists :supersede)
        (loop for line in lines
            do (format stream "~a~%" line)
        )
    )
)

(defun split-string (seperator line index result)
    (if (>= index (length line))
        result
        (split-string seperator line (+ index 1) (append result (list (subseq line index (+ index 1)))))
    )
)



(defun main (lines index converted-lines)
  (if (< index (length lines))
        (let ((line (split-string " " (line-by-index lines index) 0 '() )))
            (format t "~a~%" lines)

        )
      converted-lines
    )
)

(write-file (main (read-file "main.c") 0 '()))