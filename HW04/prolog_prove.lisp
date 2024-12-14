<<<<<<< Updated upstream



;; You will implement a simplified version of Prolog  theorem prover. Recall 
;; that Prolog uses Horne clauses (a simplified version of first order predicate calculus) to define facts. A 
;; query  is  proven  using  resolution  and  unification.  The  search  space  is  traversed  using  a  depth-first 
;; manner with sub queries are proven from left to right and axioms are searched from top to bottom. 

;; A list describing the axioms in the form (axiom_1 axiom_2 ... axiom_n). The list will have 
;; at least one axiom or more (assume that list is not empty). 
 
=======
;; PROLOG in Common  LISP:  You will implement a simplified version of Prolog  theorem prover. Recall 
;; that Prolog uses Horne clauses (a simplified version of first order predicate calculus) to define facts. A 
;; query  is  proven  using  resolution  and  unification.  The  search  space  is  traversed  using  a  depth-first 
;; manner with sub queries are proven from left to right and axioms are searched from top to bottom.  
;; You will implement a single function called prolog_ prove in Common LISP. This function will take 
;; as input two arguments
;; 
;; A list describing the axioms in the form (axiom_1 axiom_2 ... axiom_n). The list will have 
;; at least one axiom or more (assume that list is not empty). 
;;  
>>>>>>> Stashed changes
;; Each axiom (Horn clause) will follow the following format: Given 
;; • Variables are capital lettered words (strings). 
;; • Objects are small cap words. 
;; • Predicates are represented by a list with the first entry being the name of the predicate 
;; (small cap words) and the remaining entries as the arguments. We assume that there 
;; is at least one argument.  
;; Each axiom is either: 
;; • a list including a single predicate, e.g., ( ("father" "jim" "jill") ), or 
;; • a list including a predicate for the head, a “<” for implication and at least one predicate 
<<<<<<< Updated upstream
;; listing the conditions. For example ( ("parent" "X" "Y") "<" ("mother" "X" "Y") ).

;; A query is a list of axioms including no head body, e.g., ( ("ancestor" "X" "jill") ) . 
;; • We assume that we have one or zero variables in each query. 
;; Your function will return a list as answer. If the query returns false, the list will be empty/nil. Otherwise, 
;; it will have a list of entries of the form ((variable value) ...) where each entry makes the query 
;; true.


(defun is-small-caps (str)
    ;; Check if the given string is all lowercase.
    (string= str (string-downcase str)))

(defun is-valid-fact (axiom)
    ;; Check if the given axiom is a valid fact (not a rule).

    (if (null axiom)
        t
        (if (listp axiom)
            (and (is-valid-fact (car axiom))
             (is-valid-fact (cdr axiom)))
            (and (is-small-caps axiom)
                (not (string= "<" axiom)))
        )
    )
)

(defun get-facts-from-axioms (axioms)
    ;; Extract all valid facts from the list of axioms.
    ;; Each fact must not contain "<" and all items are lowercase.
    (labels ((get-facts (axioms facts)
                (if (null axioms)
                    facts
                    (let ((axiom (car axioms)))
                    (if (is-valid-fact axiom)
                        (get-facts (cdr axioms) (cons axiom facts))
                        (get-facts (cdr axioms) facts))))))
        (get-facts axioms '())))

(defun get-rules-from-axioms (axioms)
    ;; Extract all rules (those containing "<") from the list of axioms.
    (remove-if #'is-valid-fact axioms))

(defun is-in-list (term list)
    ;; Check if the given term is in the list.
    (if (null list)
        t
        (if (string= term (car list))
            nil
            (is-in-list term (cdr list))
        )
    )
)

(defun extract-variables (facts)
    ;; Extract all variables from the list of facts.
    (let ((variables '()))
        (dolist (fact facts)
            (setf fact (car fact))
            (loop for x from 1 to (1- (length fact))
                do (let ((term (nth x fact)))
                    (if (and (string= (string-downcase term) term)
                            (is-in-list term variables))
                        (setq variables (cons term variables))
                    )
                )
            )
        )
        variables)
)

(defun unify (term1 term2 variables)
    ;; Unify two terms. If they are the same or one is a variable, return a substitution. Else, fail.
    ;; terö1 and term2 are lists
)

(defun dfs (rules facts query substitution variables)
    ;; Apply DFS to recursively try to prove a query by unifying it with facts or rules.
    (cond
        ((member query facts) substitution)  ; If query matches a fact, return the substitution
        ((null rules) nil)  ; No more rules to apply, failure
        (t
            (format t "Rules: ~a~%" rules)
            (format t "car rules: ~a~%" (car rules))
            (let* ((rule (car rules))
                    (head (car rule)) ;; Head of the rule
                    (body (cdr (cdr rule)))) ;; Still A list
                    (format t "head: ~a~%" head)
                    (format t "body: ~a~%" body)
                ;; Unify the query with the head of the rule
                (let ((new-substitution (unify query head variables)))
                    (if new-substitution
                        (let ((new-body (mapcar (lambda (b) (apply-substitution b new-substitution)) body)))
                            ;; Recursively prove the body of the rule
                            (dfs rules facts new-body (append substitution new-substitution) variables)
                        )
                        nil
                    )
                )
            )    
        )
    )
)


(defun unification (axioms facts rules queries variables)
    ;; Unify the queries with the facts and rules.
    (let ((substitution '()))
        (dolist (query queries)
            (let ((result (dfs rules facts query substitution variables)))
                (if result
                    (format t "~a~%" result)
                    (format t "false~%")
                )
            )
        )
    )
)

(defun prolog-prove (axioms queries)
    (let* ((facts (get-facts-from-axioms axioms)) ;; Extract facts from axioms.
            (rules (get-rules-from-axioms axioms))
            (variables (extract-variables facts))) ;; Extract rules from axioms.
        (unification axioms facts rules queries variables)
    )
)



;; Example usage:
(let ((axioms '( (("father" "jim" "jill"))
                (("mother" "mary" "jill"))
                (("father" "samm" "jim"))
                (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                (("parent" "X" "Y") "<" ("mother" "X" "Y"))
                (("parent" "X" "Y") "<" ("father" "X" "Y"))
        ))
      (query1 '(("ancestor" "X" "jill")))
      (query2 '(("ancestor" "X" "jill") ("mother" "X" "bob"))))
  (format t "~a~%" (prolog-prove axioms query1)))
=======
;; listing the conditions. For example ( ("parent" "X" "Y") "<" ("mother" "X" 
;; "Y") ). 
;;  
;; 2. A query is a list of axioms including no head body, e.g., ( ("ancestor" "X" "jill") ) . 
;; • We assume that we have one or zero variables in each query. 
;; Your function will return a list as answer. If the query returns false, the list will be empty/nil. Otherwise, 
;; it will have a list of entries of the form ((variable value) ...) where each entry makes the query 
;; true
;; 
;; (let ( 
;;         (axioms '(  
;;                     ( ("father" "jim" "jill") ) 
;;                     ( ("mother" "mary" "jill") ) 
;;                     ( ("father" "samm" "jim") ) 
;;                     ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") ) 
;;                     ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") ) 
;;                     ( ("parent" "X" "Y") "<" ("mother" "X" "Y") ) 
;;                     ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) ) 
;;         (query1 '( ("ancestor" "X" "jill") ) )  
;;         (query2 '( ("ancestor" "X" "jill") ("mother" "X" "bob") ) ) ) 
;;     (prolog_prove axioms query1) 
;; ) 
;; will return ("X" "parent") 
;; whereas, if we change the last line to (prolog_prove axioms query2) will return nil.  
;; Handin your code in a single Lisp file named yourstudentnumber_lastname_firstname_hw4.lisp. This 
;; file should define prolog_ prove function as described above.  

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
    (if (null constants)
        nil
        (if (and (string= (first (car constants)) first_exp)
                 (string= (third (car constants)) last_exp))
            (cons (car constants) (find-constants (cdr constants) first_exp last_exp))
            (find-constants (cdr constants) first_exp last_exp))))

(defun find-all (constants rules query)
    (let ((result '()))
        (setq result (find-constants constants (first query) (third query)))
        (loop for i from 0 below (length rules) do
            (if (string= (first (nth i rules)) (first query))
                (setq result (append result (find-all constants rules (list (first (third (nth i rules)))
                    (second query) (third query)))))))
        result))

(defun execute-query (constants rules query)
    (print rules)
    (let ((result '()))
        (if (and (is-lower (first query)) (is-lower (third query)))
            (if (is-lower (second query))
                    (if (null (find-constant constants query))
                        (format nil "FALSE")
                        (format nil "TRUE"))
                (find-all constants rules query))
            (format t "SYNTAX ERROR: Query must have lower case first element and lower case third element~%"))))


(defun main (axioms query)
    (let ((constants '()) (rules '()))
        (loop for i from 0 below (length axioms) do
            (if (> (length (nth i axioms)) 1)
                (setq rules (list (append rules (nth i axioms))))
                (setq constants (append constants (nth i axioms)))))
        (if (/= (length query) 3)
            (format t "SYNTAX ERROR: Query must have 3 elements~%")
            (format t "Result: a%" (execute-query constants rules query)))))

(defvar axioms '(  
                    (("father" "jim" "jill")) 
                    (("mother" "mary" "jill")) 
                    (("father" "samm" "jim")) 
                    (("ancestor" "X" "Y") "<" ("parent" "X" "Y")) 
                    (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y")) 
                    (("parent" "X" "Y") "<" ("mother" "X" "Y")) 
                    (("parent" "X" "Y") "<" ("father" "X" "Y"))))

(defvar query1 '(("ancestor" "X" "jill")))

(main axioms query1) ; (("X" "jim") ("X" "mary") ("X" "samm"))
>>>>>>> Stashed changes
