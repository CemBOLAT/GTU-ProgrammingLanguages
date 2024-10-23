(declaim (ftype (function (integer integer) integer) sum))

(defun sum (a b) 
(progn
(+ a b)
))

(defun main () 
(progn
(setq x 10)
(setq y 20)
(setq result (sum x y))

(if (> result 25) 
(progn
(format t "Result is greater than 25~%")
(setq x 5)
))

(loop for i from 0 below 10 by 1 do
(progn
(format t "~a~%" i)
))
(format t "----------------~%")
(loop for i from 0 to 10 by 1 do
(progn
(format t "~a~%" i)
))
0
))
