(declaim (ftype (function (integer integer) integer) sum))

(defun sum (a b) 
(let* ()
    (+ a b)
))

(defun mult (a b c) 
(let* ()
    (* (* a b) c)
))

(defun print_test () 
(let* ()
    (format t "Hello World~%")
    0
))

(defun main () 
(let* (
    (x 10)
    (y 20)
    (i 0)
    (result (sum x y)))
    
    (if (> result 25)
    (progn
        (format t "Result is greater than 25~%")
        (setq x 5)
    ))

    (loop for i from 0 below 10 by 1 do
    (progn
        (format t "~a~%" i)
    ))

    (setq x (mult x y result))
    (format t "Result: ~a~%" x)

    (setq x (+ x 1))

    (loop while (> x 600) do
    (progn
        (format t "~a~%" x)
        (setq x (/ x 2))
    ))

    (print_test)

    0
))

(main)