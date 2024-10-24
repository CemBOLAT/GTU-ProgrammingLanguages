

(defun read-file (fileName)
  ;; Opens the file and returns a function that reads the next line each time it's called
  (let ((stream (open fileName :direction :input)))
    (lambda ()
      (let ((next-line (read-line stream nil)))
        (if next-line
            next-line
            (progn
              (close stream)
              nil))))))

(let ((reader (read-file "main.c")))
  (loop for line = (funcall reader)
        while line
        do (format t "Line: ~a~%" line)))
