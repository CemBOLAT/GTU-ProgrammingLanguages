(setq new_type nil)
(setq newest_type nil)
(setq other-rule nil)
(setq known-variable nil)
(setq first_unknown_variable nil)
(setq second_unknown_variable nil)

(defun is-capital-str (str)
	;; Check if the string is all capital letters
    (if (string= str (string-upcase str))
        t
        nil
	)
)

(defun is-down-str (str)
	;; Check if the string is all lowercase letters
    (if (string= str (string-downcase str))
        t
        nil
	)
)

(defun delete-nth (i rules)
	;; Delete the ith element from the list
	(let ((return-list '()))
		(loop for index from 0 below (length rules) do
			(if (not (= index i))
				(setq return-list (append return-list (list (nth index rules)))))
		)
		return-list
	)
)

(defun find-constant (all_constants query)
	;; Find the constant in the constants list
    (if (null all_constants)
        nil
        (if (and (string= (first (car all_constants)) (first query))
                     (string= (second (car all_constants)) (second query))
                     (string= (third (car all_constants)) (third query)))
                    (car all_constants)
                (find-constant (cdr all_constants) query))))

(defun find-constants (all_constants query type)
	;; Find the constants in the constants list
	(if (null all_constants)
		nil
		(let ((first-exp (first query))
			  (last-exp (third query)))
		  (if (or (and (= type 1) (string= (first (car all_constants)) first-exp) (string= (third (car all_constants)) last-exp))
				  (and (= type 2) (string= (first (car all_constants)) first-exp) (string= (second (car all_constants)) (second query))))
			  (cons (if (= type 1) (second (car all_constants)) (third (car all_constants)))
					(find-constants (cdr all_constants) query type))
			  (find-constants (cdr all_constants) query type)))))

(defun build-query (rule query type)
	;; Build the query according to the rule
	;; type 1 -> first_unknown_variable is in the first part of the rule
	(if (= type 1)
		(progn
			(setq first_unknown_variable (second (first rule)))
			(setq known-variable (third (first rule)))))
	;; type 2 -> first_unknown_variable is in the third part of the rule
	(if (= type 2)
		(progn
			(setq first_unknown_variable (third (first rule)))
			(setq known-variable (second (first rule)))))
	;; Find the extrernal rule
	(loop for i from 2 below (length rule) do
		(if (string= (second (nth i rule)) known-variable)
			(progn
				(if (= i 2)
					(setq other-rule (nth 3 rule))
					(setq other-rule (nth 2 rule)))
				(setq new_type 2) ;; new_type 2 -> first_unknown_variable is in the third part of the rule
				(return (list (first (nth i rule)) (second query) (third (nth i rule))))
			)
		)
		(if (string= (third (nth i rule)) known-variable)
			(progn
				(if (= i 2)
					(setq other-rule (nth 3 rule))
					(setq other-rule (nth 2 rule)))
				(setq new_type 1) ;; new_type 1 -> first_unknown_variable is in the first part of the rule
				(return (list (first (nth i rule)) (second (nth i rule)) (third query)))
			)
		)
	)
)

(defun append-query (to from)
	;; Append the query to the list
	(loop for i from 0 below (length from) do
		(if (not (member (nth i from) to :test #'string=))
			(setq to (append to (list (nth i from))))
		))
	to
)

(defun intersect-query (l1 l2)
	;; Intersect the lists
	(let ((result '()))
		(loop for i from 0 below (length l1) do
			(if (member (nth i l1) l2 :test #'string=)
				(setq result (append result (list (nth i l1))))
			)
		)
		result
	)
)

(defun query-builder-for-other (lst)
	;; Build the query for the other rule
	(if (string= (second other-rule) first_unknown_variable)
		(setq newest_type 1) ;; new_type 1 -> first_unknown_variable is in the first part of the rule
		(setq newest_type 2)) ;; new_type 2 -> first_unknown_variable is in the third part of the rule
	(if (string= (second other-rule) first_unknown_variable)
		(list (first other-rule) (second other-rule) lst)
		(list (first other-rule) lst (third other-rule))))

(defun find-all-executed-query (constants rules query type)
	;; Find all the executed queries
	(let ((result '()))
		(setq result (find-constants constants query type))
		(loop for i from 0 below (length rules) do
			(if (string= (first (first (nth i rules))) (first query))
				(cond 
					((= (length (nth i rules)) 3)
						(setq result (append-query result (find-all-executed-query constants rules (list (first (third (nth i rules))) (second query) (third query)) type))))
					((= (length (nth i rules)) 4)
						(let* ((nthrule (nth i rules))
							(deleted-rules (delete-nth i rules))
							(builed-lst (find-all-executed-query constants deleted-rules (build-query nthrule query type) new_type)))
							(loop for j from 0 below (length builed-lst) do
								(setq result (append-query result (find-all-executed-query constants rules (query-builder-for-other (nth j builed-lst)) newest_type)))
							)
						)
					)
				)
			)
		)
		result
	)
)

(defun execute-query (constants rules query)
	;; Execute the query and return the result
	(if (is-down-str (first query))
		(progn
			(if (and (is-down-str (second query)) (is-down-str (third query)))
				(if (null (find-constant constants query))
					(if (member (second query) (find-all-executed-query constants rules (list (first query) "X" (third query)) 1) :test #'string=)
						(return-from execute-query (list "True"))
						(return-from execute-query (list "nil")))
					))
			(if (and (is-capital-str (second query)) (is-down-str (third query)))
				(progn
					(setq second_unknown_variable (second query))
					(return-from execute-query (find-all-executed-query constants rules query 1))
				)
			)
			(if (and (is-down-str (second query)) (is-capital-str (third query)))
				(progn
					(setq second_unknown_variable (third query))
					(return-from execute-query (find-all-executed-query constants rules query 2))
				)
			)
		)
	)
)
	
(defun final-result-fixer (result)
	;; Fix the result to be more readable
	(if (null result)
		nil
	)
	(if (or (string= (first result) "True") (string= (first result) "nil"))
		(return-from final-result-fixer (first result))
	)
	(let ((list_return '()))
		(loop for i from 0 below (length result) do
			(setq list_return (append list_return (list (list second_unknown_variable (nth i result)))))
		)
		list_return
	)
)

(defun prolog_prove (axioms query)
	;; Prolog_prove function
	;; Find the constants and rules
	;; Execute the queries
	;; return the intersected result
    (let ((constants '()) (rules '()) (result '()))
        (loop for i from 0 below (length axioms) do
            (if (= (length (nth i axioms)) 1)
                (setq constants (append constants (nth i axioms)))
                (setq rules (append rules (list (nth i axioms))))
			)
		)
        (loop for i from 0 below (length query) do
            (if (= i 0)
                (setq result (execute-query constants rules (nth i query)))
					
                (setq result (intersect-query result (execute-query constants rules (nth i query))))
			)
		)
        (return-from prolog_prove (final-result-fixer result))
    )
)

(let (
	(axioms '(
		(("father" "jim" "jill"))
		(("mother" "anna" "jill"))
		(("father" "sam" "jim"))
		(("mother" "lisa" "anna"))
		(("father" "john" "sam"))
		(("parent" "X" "Y") "<" ("father" "X" "Y"))
		(("parent" "X" "Y") "<" ("mother" "X" "Y"))
		(("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
		(("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))))
	(query1 '( ("ancestor" "X" "jill") ) )
	(query2 '( ("parent" "X" "jill") ) ))
	(format t "Query 1: ~a~%" (prolog_prove axioms query1))
	(format t "Query 2: ~a~%" (prolog_prove axioms query2))
)