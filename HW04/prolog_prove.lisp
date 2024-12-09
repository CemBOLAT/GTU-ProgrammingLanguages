


;; You will implement a simplified version of Prolog  theorem prover. Recall 
;; that Prolog uses Horne clauses (a simplified version of first order predicate calculus) to define facts. A 
;; query  is  proven  using  resolution  and  unification.  The  search  space  is  traversed  using  a  depth-first 
;; manner with sub queries are proven from left to right and axioms are searched from top to bottom. 

;; A list describing the axioms in the form (axiom_1 axiom_2 ... axiom_n). The list will have 
;; at least one axiom or more (assume that list is not empty). 
 
;; Each axiom (Horn clause) will follow the following format: Given 
;; • Variables are capital lettered words (strings). 
;; • Objects are small cap words. 
;; • Predicates are represented by a list with the first entry being the name of the predicate 
;; (small cap words) and the remaining entries as the arguments. We assume that there 
;; is at least one argument.  
;; Each axiom is either: 
;; • a list including a single predicate, e.g., ( ("father" "jim" "jill") ), or 
;; • a list including a predicate for the head, a “<” for implication and at least one predicate 
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