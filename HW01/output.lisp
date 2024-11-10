(declaim (ftype (function (integer integer) integer) sum))

(defun sum (a b) 
(let* ()
    (+ a b)
))

(defun mult (a b c) 
(let* ()
    (+ a (* b c))
))

(defun div (a b) 
(let* ()
    (/ a b)
))

(defun pow (a b) 
(let* (
    (result 1)
    (i 0))
    (loop for i from 0 below b by 1 do
    (progn
        (setq result (* result a))
    ))
    result
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
    (result (sum x y))
    (power (pow 2 3)))
    
    (if (> result 25)
    (progn
        (format t "Result is greater than 25~%")
        (setq x 5)
    ))

    (if (or (and (> x 5) (< x 30)) (< y 30))
    (progn
        (format t "x is greater than 5 and y is less than 30~%")
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

    (format t "Power: ~a~%" power)

    0
))
