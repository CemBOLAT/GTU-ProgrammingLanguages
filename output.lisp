
(declaim (ftype (function () int) func1))
(declaim (ftype (function (integer integer) integer) func2))
(declaim (ftype (function (integer integer integer) integer) func3))
(declaim (ftype (function (integer integer integer double-float) double-float) func4))
(declaim (ftype (function (character character character character) nil) func5))
(declaim (ftype (function (void character character character) nil) func5))


(defun func4 (a b c d)
nil
)

(defun main ()
(setq cemal (+ 2 (+ 2 (* 2 2))))
(setq cemal (func 2 4 5 ))
(setq cemal (func))
(setq cemal 2)
(setq cemal (+ 23 (+ 10 (* 2 24))))
(setq cemal (+ 23 (* (+ 2 10) 24)))
(setq ela (+ (+ 10 23) (* 2 24)))
(if (eq cemal (and 2 (eq cemal 3)))
(setq cemal 2)
)

(loop while (eq cemal 2) do
(setq cemal 2)
(setq cemal (+ cemal 2))
)
(loop for a from 2 below 10 by 1 do
(setq cemal 2)
)
0
)
