(setq unknown-var nil)
(setq unknown-var2 nil)
(setq known-var nil)
(setq other-rule nil)
(setq newtip nil)
(setq newnewtip nil)

(defun is-upper (str)
    (if (string= str (string-upcase str))
        t
        nil))

(defun is-lower (str)
    (if (string= str (string-downcase str))
        t
        nil))

(defun except-nth (i rules)
	(let ((newlist '()))
		(loop for j from 0 below (length rules) do
			(if (not (= i j))
				(setq newlist (append newlist (list (nth j rules)))))
		)
		newlist))

(defun find-constant (constants query)
    (if (null constants)
        nil
        (if(and (string= (first (car constants)) (first query))
                     (string= (second (car constants)) (second query))
                     (string= (third (car constants)) (third query)))
                    (car constants)
                (find-constant (cdr constants) query))))

(defun find-constants (constants query tip)
	(if (null constants)
		nil
		(let ((first-exp (first query))
			  (last-exp (third query)))
		  (if (or (and (= tip 1) (string= (first (car constants)) first-exp) (string= (third (car constants)) last-exp))
				  (and (= tip 2) (string= (first (car constants)) first-exp) (string= (second (car constants)) (second query))))
			  (cons (if (= tip 1) (second (car constants)) (third (car constants)))
					(find-constants (cdr constants) query tip))
			  (find-constants (cdr constants) query tip)))))

(defun build-query (rule query tip)
	(if (= tip 1)
		(progn
			(setq unknown-var (second (first rule)))
			(setq known-var (third (first rule)))))
	(if (= tip 2)
		(progn
			(setq unknown-var (third (first rule)))
			(setq known-var (second (first rule)))))

	(loop for i from 2 below (length rule) do
		(if (string= (second (nth i rule)) known-var)
			(progn
				(if (= i 2)
					(setq other-rule (nth 3 rule))
					(setq other-rule (nth 2 rule)))
				(setq newtip 2)
				(return (list (first (nth i rule)) (second query) (third (nth i rule))))
			)
		)
		(if (string= (third (nth i rule)) known-var)
			(progn
				(if (= i 2)
					(setq other-rule (nth 3 rule))
					(setq other-rule (nth 2 rule)))
				(setq newtip 1)
				(return (list (first (nth i rule)) (second (nth i rule)) (third query)))
			)
		)
	)
)

(defun my-append (list1 list2)
	(loop for i from 0 below (length list2) do
		(if (not (member (nth i list2) list1 :test #'string=))
			(setq list1 (append list1 (list (nth i list2))))
		))
	list1)

(defun my-intersect (list1 list2)
	(let ((newlist '()))
		(loop for i from 0 below (length list1) do
			(if (member (nth i list1) list2 :test #'string=)
				(setq newlist (append newlist (list (nth i list1))))
			)
		)
		newlist))

(defun build-query2 (newlist)
	(if (string= (second other-rule) unknown-var)
		(setq newnewtip 1)
		(setq newnewtip 2))
	(if (string= (second other-rule) unknown-var)
		(list (first other-rule) (second other-rule) newlist)
		(list (first other-rule) newlist (third other-rule))))

(defun find-all (constants rules query tip)
  (let ((result '()))
    (setq result (find-constants constants query tip))
    (loop for i from 0 below (length rules) do
      (if (string= (first (first (nth i rules))) (first query))
          (cond 
           ((= (length (nth i rules)) 3)
            (setq result (my-append result (find-all constants rules (list (first (third (nth i rules))) (second query) (third query)) tip))))
           ((= (length (nth i rules)) 4)
            (let* ((nthrule (nth i rules))
                   (deleted-rules (except-nth i rules))
				   (newlist (find-all constants deleted-rules (build-query nthrule query tip) newtip)))
			  (loop for j from 0 below (length newlist) do
			  	(setq result (my-append result (find-all constants rules (build-query2 (nth j newlist)) newnewtip)))
			  ))))))
    result))

(defun execute-query (constants rules query)
	(if (is-lower (first query))
		(progn
			(if (and (is-lower (second query)) (is-lower (third query)))
				(if (null (find-constant constants query))
					(if (member (second query) (find-all constants rules (list (first query) "X" (third query)) 1) :test #'string=)
						(return-from execute-query (list "True"))
						(return-from execute-query (list "False")))
					))
			(if (and (is-upper (second query)) (is-lower (third query)))
			(progn
				(setq unknown-var2 (second query))
				(return-from execute-query (find-all constants rules query 1))))
			(if (and (is-lower (second query)) (is-upper (third query)))
			(progn
				(setq unknown-var2 (third query))
				(return-from execute-query (find-all constants rules query 2))))))
	)
	
(defun final-fix (result)
	(if (null result)
		nil)
	(if (or (string= (first result) "True") (string= (first result) "False"))
		(return-from final-fix (first result)))
	(let ((newlist '()))
		(loop for i from 0 below (length result) do
			(setq newlist (append newlist (list (list unknown-var2 (nth i result))))))
			newlist))

(defun prolog_prove (axioms query)
    (let ((constants '()) (rules '()) (result '()))
        (loop for i from 0 below (length axioms) do
            (if (= (length (nth i axioms)) 1)
                (setq constants (append constants (nth i axioms)))
                (setq rules (append rules (list (nth i axioms))))))
        (loop for i from 0 below (length query) do
            (if (= i 0)
                (setq result (execute-query constants rules (nth i query)))
                (setq result (my-intersect result (execute-query constants rules (nth i query))))))
        (return-from prolog_prove (final-fix result))
    )
)

(let ((axioms '(
			( ("father" "cemal" "dadat"))
			( ("father" "cemal" "leyla"))
			( ("father" "hasan" "ela"))
			( ("mother" "güleser" "ela"))
			( ("father" "hasan" "cemal"))
			( ("mother" "güleser" "cemal"))
			( ("mother" "necmettin" "güleser"))
			( ("mother" "necmettin" "sadik"))
			( ("mother" "emineanneanne" "güleser"))
			( ("mother" "emineanneanne" "sadik"))
			( ("father" "dedecemal" "hasan"))
			( ("mother" "emine" "hasan"))
			( ("mother" "emine" "pakize"))
			( ("father" "dedecemal" "pakize"))
			( ("father" "dedecemal" "ismail"))
			( ("mother" "emine" "ismail"))
			( ("mother" "sule" "jose"))
			( ("father" "ismail" "jose"))
			( ("mother" "sule" "emineyig"))
			( ("father" "ismail" "emineyig"))
			( ("mother" "sule" "eylul"))
			( ("father" "ismail" "eylul"))

			( ("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
			( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )
			( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
			( ("parent" "X" "Y") "<" ("father" "X" "Y") )

			( ("sibling" "X" "Y") "<" ("parent" "Z" "X") ("parent" "Z" "Y"))
			( ("brother" "X" "Y") "<" ("parent" "Z" "X") ("parent" "Z" "Y") )
		))
		(query4 '( ("ancestor" "X" "cemal") ) )
		(query5 '( ("ancestor" "cemal" "X") ) )
	)
	(format t "Query4: ~a~%" (prolog_prove axioms query4))
	(format t "Query5: ~a~%" (prolog_prove axioms query5))
)