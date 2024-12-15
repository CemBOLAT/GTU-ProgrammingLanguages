;; this lisp program takes 2 arguments, one is prolog axiom list and the other is a query to execute and returns the result of the query

(defun is-upper (str)
    (if (string= str (string-upcase str))
        t
        nil))

(defun is-lower (str)
    (if (string= str (string-downcase str))
        t
        nil))

(defun find-constant (constants query)
    (if (null constants)
        nil
        (if(and (string= (first (car constants)) (first query))
                     (string= (second (car constants)) (second query))
                     (string= (third (car constants)) (third query)))
                    (car constants)
                (find-constant (cdr constants) query))))

(defun find-constants (constants first_exp last_exp)
    (print (format nil "first_exp: ~a, last_exp: ~a" first_exp last_exp))
    (if (null constants)
        nil
        (if (and (string= (first (car constants)) first_exp)
                 (string= (third (car constants)) last_exp))
            (cons (car constants) (find-constants (cdr constants) first_exp last_exp))
            (find-constants (cdr constants) first_exp last_exp))))

(defun find-all (constants rules query)
    (print (format nil "query: ~a" query))
    (let ((result '()))
        (setq result (find-constants constants (first query) (third query)))
        (loop for i from 0 below (length rules) do
            (if (string= (first (first (nth i rules))) (first query))
                (cond 
                    ((= (length (nth i rules)) 3) ;; in case of simple rule
                        (setq result (append result (find-all constants rules (list (first (third (nth i rules))) (second query) (third query))))))
                    ((= (length (nth i rules)) 4) ;; in case of connected rule
                        (defvar nthrule (nth i rules))
                        (defvar newlist (find-all constants (delete (nth i rules) rules) (list (first (first (last nthrule))) (second query) (third query))))
                        (print (format nil "newlist: ~a" newlist))
                        (loop for j from 0 below (length newlist) do
                            (setq result (append result (find-all constants rules (list (first (third nthrule)) (second query) (second (nth j newlist))))))))
                    (t result)
                )
            ))
        result))

(defun execute-query (constants rules query)
    (let ((result '()))
        (if (and (is-lower (first query)) (is-lower (third query)))
            (if (is-lower (second query))
                    (if (null (find-constant constants query))
                        (format nil "FALSE")
                        (format nil "TRUE"))
                (find-all constants rules query))
            (format t "SYNTAX ERROR: Query must have lower case first element and lower case third element~%"))))

(defun extract-person (result)
    (let ((person '()))
        (loop for i from 0 below (length result) do
            (setq person (append person (list (second (nth i result)))))
        )
        person))

(defun main (axioms query)
    (let ((constants '()) (rules '()))
        (loop for i from 0 below (length axioms) do
            (if (= (length (nth i axioms)) 1)  ; Check if it's a constant
                (setq constants (append constants (nth i axioms)))
                (setq rules (append rules (list (nth i axioms))))))
        (if (/= (length query) 3)
            (format t "SYNTAX ERROR: Query must have 3 elements~%")
            (format t "~%~%Result: ~a~%" (extract-person (execute-query constants rules query))))))


(main '((("father" "jim" "jill")) (("mother" "mary" "jill")) (("father" "samm" "jim"))
        (("mother" "susan" "jim")) (("father" "jim" "joe")) (("mother" "mary" "joe"))
        (("father" "kevin" "susan")) (("mother" "ellie" "mary"))
        (("parent" "X" "Y") "<" ("father" "X" "Y")) (("parent" "X" "Y") "<" ("mother" "X" "Y"))
        (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
        (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y")))
    '("ancestor" "X" "jill"))
    ;; jill' mother is mary, father is jim, 
    ;; joe's mother is mary, father is jim,
    ;; jim's mother is susan, father is samm
    ;; susans's father is kevin
    ;; mary's mother is ellie