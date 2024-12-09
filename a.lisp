(defun unify (term1 term2)
  "Unify two terms. If they are the same or one is a variable, return a substitution. Else, fail."
  (cond
    ((string= term1 term2) nil)  ; No substitution needed
    ((and (string= (substring term1 0 1) "?")  ; term1 is a variable
          (not (string= term1 term2)))  ; term2 is not the same as term1
     (list (cons term1 term2)))  ; unify the variable with term2
    ((and (string= (substring term2 0 1) "?")  ; term2 is a variable
          (not (string= term1 term2)))  ; term1 is not the same as term2
     (list (cons term2 term1)))  ; unify the variable with term1
    (t nil)))  ; If no unification is possible, return failure

(defun apply-substitution (term substitution)
  "Apply a list of substitutions to a term."
  (let ((result term))
    (dolist (substitution-pair substitution result)
      (setq result (replace-regexp-in-string (car substitution-pair) (cdr substitution-pair) result)))))


(defun dfs (rules facts query substitution)
  "Apply DFS to recursively try to prove a query by unifying it with facts or rules."
  (cond
    ((member query facts) substitution)  ; If query matches a fact, return the substitution
    ((null rules) nil)  ; No more rules to apply, failure
    (t
     (let ((rule (car rules))
           (head (car rule))
           (body (cdr rule)))
       ;; Unify the query with the head of the rule
       (let ((new-substitution (unify query head)))
         (if new-substitution
             (let ((new-body (mapcar (lambda (b) (apply-substitution b new-substitution)) body)))
               ;; Recursively prove the body of the rule
               (dfs rules facts new-body (append substitution new-substitution)))
             nil)))))

