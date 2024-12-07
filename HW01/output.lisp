(< #include stdio.h)

(declaim (ftype (function (integer integer) integer) weighted_sum))

(defun main () 
(let* (
    (x 10)
    (y 20)
    (sum 0))

    (setq sum (weighted_sum x y))
    
    (format t "The sum is: ~a~%" sum)
    0
))

(defun weighted_sum (a b) 
(let* (
    (result 0)
    (i 0))
    (loop for i from 0 below a by 1 do
    (progn
        (if (eq (mod i 2) 0)
        (progn
            (setq result + i)
        ))
    ))
    (setq i 0)
    (loop while (< i b) do
    (progn
        (if (/= (mod i 2) 0)
        (progn
            (setq result + i)
        ))
        (+ (+ i ==) )
    ))
    result
))
