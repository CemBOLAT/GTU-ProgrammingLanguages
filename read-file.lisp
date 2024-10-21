;; Product Rules:
;; <line> ::= <if-statement> | <for-loop> | <while-loop> | <variable-assignment> | <fuction-defination> | <fuction-declaration> | <return-statement> | <variable-re-assignment> | <end-block> | <empty-line>
;; <if-statement> ::= if <space> ( <space> <logical-expression> <space> ) <space> { <line>* }
;; <for-loop> ::= for <space> ( <space> <variable-assignment> <space> ; <space> <logical-expression> <space> ; <space> <arithmetic-expr> <space> ) <space> { <line>* }
;; <while-loop> ::= while <space> ( <space> <logical-expression> <space> ) <space> { <line>* }
;; <variable-assignment> ::= <data-type> <space> <param-name> <space> = <space> <arithmetic-expr> <space> ; | <data-type> <space> <param-name> <space> = <space> <func-name> <space> (<function-call-params>) <space> ;
;; <fuction-defination> ::= <data-type> <space> <func-name> <space> (<function-decleration-params>) <space> { <line>* }
;; <fuction-declaration> ::= <data-type> <space> <func-name> <space> (<function-decleration-params>) <space> ;
;; <return-statement> ::= return <space> <arithmetical-expr> <space> ;
;; <variable-re-assignment> ::= <param-name> <space> = <space> <arithmetic-expr> <space> ;
;; <end-block> ::= }
;; <empty-line> ::= <space> | <new-line> | <tab>
;; <logical-expression> ::= <literal> <space> <logical-operator> <space> <literal> | <literal> <space> <logical-operator> <space> <logical-expression>
;; <logical-operator> ::= && | || | !
;; <literal> ::= <param-name> | <number>
;; <arithmetic-expr> ::= <term> <space> <+> <space> <term> | <term> <space> <-> <space> <term> | <term>
;; <term> ::= <factor> <space> <*> <space> <factor> | <factor> <space> / <space> <factor> | <factor>
;; <factor> ::= <number> | <param-name> | <space> ( <space> <arithmetic-expr> <space> )
;; <function-decleration-params> ::= <data-type> <space> <param-name> | <data-type> <space> <param-name> , <space> <function-decleration-params>
;; <function-call-params> ::= <param-name> | <param-name> , <space> <function-call-params>
;; <data-type> ::= int | float | double | char | void
;; <param-name> ::= <letter> | <letter> <param-name> | <letter> <number> | <letter> <number> <param-name>
;; <func-name> ::= <letter> | <letter> <func-name> | <letter> <number> | <letter> <number> <func-name>
;; <number> ::= <digit> | <digit> <number>


(defun list-to-string (list)
    ;; converts a list of strings to a single string
    (apply #'concatenate 'string list)
)

(defun line-by-index (lines index)
    ;; returns the line at the given index
    (nth index lines)
)

(defun is-variable-type (line)
    ;; checks if the given string is equal to one of the variable types in C
    (cond
        ((string= line "int") t)
        ((string= line "float") t)
        ((string= line "double") t)
        ((string= line "char") t)
        ((string= line "void") t)
        (t nil)
    )
)


(defun split-string (separator line index result token is-splittable)
    ;; splits a string by the given separator and returns a list of the parts
    (let ((separator-length (length separator)))
        (if (>= index (length line))
            (if (string= token "")
                result
                (append result (list token)))
            (let ((current-substr (subseq line index (min (+ index separator-length) (length line)))))
                (if (string= current-substr separator)
                    (split-string separator line (+ index separator-length) 
                                (if (string= token "")
                                    result
                                    (append result (list token))) 
                                "" nil)
                    (split-string separator line (+ index 1) 
                            result 
                            (concatenate 'string token (subseq line index (+ index 1))) t)
                )
            )
        )
    )
)

(defun evaluate-function-call (expr-list index)
    (if (>= index (length expr-list))
        ""
        (concatenate 'string (nth index expr-list) " " (evaluate-function-call expr-list (+ index 1)) )
    )
)


;; öncelik kısmı eksik brom.
(defun evaluate-infix-arithmetic-expression (infix operator-stack output-queue index)
    (if (>= index (length infix))
        (let* ((op (pop operator-stack)) (operand1 (pop output-queue)) (operand2 (pop output-queue)))
            (if (null op)
                (progn
                    (push (concatenate 'string operand1) output-queue)
                    (list-to-string output-queue)
                )
                (progn
                    (push (concatenate 'string "(" op " " operand2 " " operand1 ")") output-queue)
                    (if (null operator-stack)
                        (list-to-string output-queue)
                        (evaluate-infix-arithmetic-expression infix operator-stack output-queue (+ index 1))
                    )
                )
            )
        )
        (let ((current-char (nth index infix)))
            (cond
                ((string= current-char "+") (push current-char operator-stack))
                ((string= current-char "-") (push current-char operator-stack))
                ((string= current-char "*") (push current-char operator-stack))
                ((string= current-char "/") (push current-char operator-stack))
                ((string= current-char "%") (push current-char operator-stack))
                ((string= current-char "(") (push current-char operator-stack))
                ((string= current-char ")") (loop for i from 0 to (- (length operator-stack) 1) do
                                                (let ((operator (pop operator-stack)))
                                                    (if (string= operator "(")
                                                        (return)
                                                        (push (concatenate 'string "(" operator " " (pop output-queue) " " (pop output-queue) ")") output-queue)
                                                    )
                                                    (evaluate-infix-arithmetic-expression infix operator-stack output-queue (+ index 1)))
                                            )
                                                
                )
                (t (push current-char output-queue))
            )
            (evaluate-infix-arithmetic-expression infix operator-stack output-queue (+ index 1))
        )
    )
)

;dont forget öncelik.
(defun evaluate-infix-logical-expression (infix operator-stack output-queue index)
    (if (>= index (length infix))
        (let* ((op (pop operator-stack)) (operand1 (pop output-queue)) (operand2 (pop output-queue)))
            (if (null op)
                (progn
                    (push (concatenate 'string operand1) output-queue)
                    (list-to-string output-queue)
                )
                (progn
                    (push (concatenate 'string "(" op " " operand2 " " operand1 ")") output-queue)
                    (if (null operator-stack)
                        (list-to-string output-queue)
                        (evaluate-infix-logical-expression infix operator-stack output-queue (+ index 1))
                    )
                )
            )
        )
        (let ((current-char (nth index infix)))
            (cond
                ; push and keyword as a string
                ((string= current-char "&&") (push "and" operator-stack))
                ((string= current-char "||") (push "or" operator-stack))
                ((string= current-char "!") (push "not" operator-stack))
                ((string= current-char "<") (push current-char operator-stack))
                ((string= current-char ">") (push current-char operator-stack))
                ((string= current-char "<=") (push current-char operator-stack))
                ((string= current-char ">=") (push current-char operator-stack))
                ((string= current-char "==") (push "eq" operator-stack))
                ((string= current-char "!=") (push "/=" operator-stack))
                ((string= current-char "(") (push current-char operator-stack))
                ((string= current-char ")") (loop for i from 0 to (- (length operator-stack) 1) do
                                                (let ((operator (pop operator-stack)))
                                                    (if (string= operator "(")
                                                        (return)
                                                        (push (concatenate 'string "(" operator " " (pop output-queue) " " (pop output-queue) ")") output-queue)
                                                    )
                                                    (evaluate-infix-logical-expression infix operator-stack output-queue (+ index 1)))
                                            )                                                
                )
                (t (push current-char output-queue))
            )
            (evaluate-infix-logical-expression infix operator-stack output-queue (+ index 1))
        )
    )
)

(defun convert-if-statement (line)
    ;; converts an if statement to a lisp if statement
    ;; (if <logical-expression> 
    ;;    <line>*)
    (let* ((logical-expression (subseq line 2 (- (length line) 2))))
        (concatenate 'string "(if " (evaluate-infix-logical-expression logical-expression '() '() 0))
    )
)

(defun extract-expr (line-str index-of-comma)
    (let ((split-parts (split-string ";" line-str 0 nil "" nil)))
        (nth (- index-of-comma 1) split-parts)))

(defun extract-var-name (variable-name)
    (cond
        ((and (> (length variable-name) 3) (string= (subseq variable-name 0 3) "int")) (subseq variable-name 3 (length variable-name)))
        ((and (> (length variable-name) 4) (string= (subseq variable-name 0 3) "char")) (subseq variable-name 4 (length variable-name)))
        ((and (> (length variable-name) 4) (string= (subseq variable-name 0 3) "void")) (subseq variable-name 4 (length variable-name)))
        ((and (> (length variable-name) 5) (string= (subseq variable-name 0 3) "float")) (subseq variable-name 5 (length variable-name)))
        ((and (> (length variable-name) 6) (string= (subseq variable-name 0 3) "double")) (subseq variable-name 6 (length variable-name)))
    )
)

(defun extract-end-value (expr varname)
    ; a<10 veya a>=10 veya a<=10 veya a>10 veya 10>a veya 10>=a veya 10<=a veya 10<a buradan hep 10 almamız lazım
    (cond
        ((search ">=" expr) (progn
                                (let* ((split-parts (split-string ">=" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
        ((search "<=" expr) (progn
                                (let* ((split-parts (split-string "<=" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
        ((search ">" expr) (progn
                                (let* ((split-parts (split-string ">" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
        ((search "<" expr) (progn
                                (let* ((split-parts (split-string "<" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
    )     
)

(defun extract-relational-operator (expr)
    (cond
        ((search ">=" expr) ">=")
        ((search "<=" expr) "<=")
        ((search ">" expr) ">")
        ((search "<" expr) "<")
    )
)

(defun extract-increment-value (expr)
    (cond
        ((search "++" expr) "1")
        ((search "--" expr) "-1")
        ((search "+" expr) (progn
                                (let* ((split-parts (split-string "+" expr 0 nil "" nil)))
                                    (nth 1 split-parts)
                                )
                            ))
        ((search "-" expr) (progn
                                (let* ((split-parts (split-string "-" expr 0 nil "" nil)))
                                    (nth 1 split-parts)
                                )
                            ))
    )
)

(defun convert-for-loop (line)
  (let* ((line-str (list-to-string line))
         (open-paren-pos (position #\( line-str))
         (close-paren-pos (position #\) line-str))
         (inside-parens (subseq line-str (+ open-paren-pos 1) close-paren-pos))
         (expr1 (extract-expr inside-parens 1))
         (expr2 (extract-expr inside-parens 2))
         (expr3 (extract-expr inside-parens 3))
         (variable-name (subseq expr1 0 (position #\= expr1)))
         (extracted-variable-name (extract-var-name variable-name))
         (start-value (subseq expr1 (+ (position #\= expr1) 1)))
         (end-value (extract-end-value expr2 extracted-variable-name))
         (relational-operator (extract-relational-operator expr2))
         (increment-value (extract-increment-value expr3))
        )
        (cond
            ((string= relational-operator "<") (concatenate 'string "(loop for " extracted-variable-name " from " start-value " below " end-value " by " increment-value " do"))
            ((string= relational-operator ">") (concatenate 'string "(loop for " extracted-variable-name " from " start-value " downto " "( + " end-value " 1)" " by " increment-value " do"))
            ((string= relational-operator ">=") (concatenate 'string "(loop for " extracted-variable-name " from " start-value " downto " end-value " by " increment-value " do"))
            ((string= relational-operator "<=") (concatenate 'string "(loop for " extracted-variable-name " from " start-value " to " end-value " by " increment-value " do"))
        )
    )
)

(defun convert-while-loop (line)
    (let ((logical-expression (subseq line 2 (- (length line) 2))))
        (concatenate 'string "(loop while " (evaluate-infix-logical-expression logical-expression '() '() 0) " do")
    )
)

;; assignent ::= <data-type> <param-name> = <expr>
;; <expr> ::= <func-call>  | <arithmetic expr>
;; <aritmetic>
(defun convert-variable-assignment (line)
    ;; it has 2 change one is arithmetic expression or function call
    ;; (let* ((param-name (nth 1 line)) (expr (list-to-string (cdr (cdr (cdr line))))))
    (let* ((param-name (nth 1 line)) (func-name (nth 3 line)) (expr (subseq line 3 (- (length line) 1))))
        (if (search "," (list-to-string expr))
            (progn
                (let* ((expr-str (list-to-string (cdr expr))) (expr-without-parantesis (subseq expr-str 1 (- (length expr-str) 1)))
                    (expr (split-string "," expr-without-parantesis 0 '() "" nil)))
                    (concatenate 'string "(setq " param-name " (" func-name " " (evaluate-function-call expr 0) "))")
                )
            )
            (if (string= (nth 4 line) "()")
                (concatenate 'string "(setq " param-name " (" func-name "))")
                (concatenate 'string "(setq " param-name " " (evaluate-infix-arithmetic-expression expr '() '() 0) ")")
            )
        )
    )
)

(defun convert-variable-re-assignment (line)
    ;; it has 2 change one is arithmetic expression or function call
    ;; (let* ((param-name (nth 1 line)) (expr (list-to-string (cdr (cdr (cdr line))))))
    (let* ((param-name (nth 0 line)) (func-name (nth 2 line)) (expr (subseq line 2 (- (length line) 1))))
        (if (search "," (list-to-string expr))
            (progn
                (let* ((expr-str (list-to-string (cdr expr))) (expr-without-parantesis (subseq expr-str 1 (- (length expr-str) 1)))
                    (expr (split-string "," expr-without-parantesis 0 '() "" nil)))
                    (concatenate 'string "(setq " param-name " (" func-name " " (evaluate-function-call expr 0) "))")
                )
            )
            (if (string= (nth 4 line) "()")
                (concatenate 'string "(setq " param-name " (" func-name "))")
                (concatenate 'string "(setq " param-name " " (evaluate-infix-arithmetic-expression expr '() '() 0) ")")
            )
        )
    )

)

(defun defition-func-parameters (parameters)
    (if (null parameters)
        ""
        (cond
            ((string= (subseq (car parameters) 0 3) "int") 
                (concatenate 'string (subseq (car parameters) 3 (length (car parameters))) " " (defition-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 4) "char") 
                (concatenate 'string (subseq (car parameters) 4 (length (car parameters))) " " (defition-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 4) "void") 
                (concatenate 'string (subseq (car parameters) 4 (length (car parameters))) " " (defition-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 5) "float")
                (concatenate 'string (subseq (car parameters) 5 (length (car parameters))) " " (defition-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 6) "double") 
                (concatenate 'string (subseq (car parameters) 6 (length (car parameters))) " " (defition-func-parameters (cdr parameters))))
        )
    )
)

(defun convert-fuction-defination (line)
    (let* ((func-return-type (nth 0 line))
            (func-name (nth 1 line))
            (func-parameters (subseq line 2 (- (length line) 1)))
            (func-parameters-string (list-to-string func-parameters)))
            (cond
                ((string= func-parameters-string "()") (concatenate 'string "(defun " func-name " ()"))
                (t
                    (let* ((func-parameters-clean (subseq func-parameters-string 1 (- (length func-parameters-string) 1)))
                            (func-parameters-list (split-string "," func-parameters-clean 0 '() "" nil))
                            (func-parameters-string (defition-func-parameters func-parameters-list)))
                        (concatenate 'string "(defun " func-name " (" (string-trim " " func-parameters-string) ")"))
                    )
            )
    )
)

(defun declare-func-parameters (parameters)
    ;; checks the first element of the list if it start with variable type then it converts it to lisp and add to string then do recursion
    (if (null parameters)
        ""
        (cond
            ((string= (subseq (car parameters) 0 3) "int") (concatenate 'string "integer " (declare-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 4) "char") (concatenate 'string "character " (declare-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 4) "void") (concatenate 'string "void " (declare-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 5) "float") (concatenate 'string "single-float " (declare-func-parameters (cdr parameters))))
            ((string= (subseq (car parameters) 0 6) "double") (concatenate 'string "double-float " (declare-func-parameters (cdr parameters)))))))

(defun convert-fuction-declaration (line)
    ;; converts a function declaration to a lisp function declaration
    ;; int func1 (int a, int b, int c) ;
    ;; return type is the first element of the line
    ;; function name is the second element of the line
    ;; function parameters are the rest of the elements of the line
    ;; func-parameters-string is the string representation of the function parameters (inta,intb,intc)
    (let* ((func-return-type (nth 0 line))
            (func-name (nth 1 line))
            (func-parameters (subseq line 2 (- (length line) 1)))
            (func-parameters-string (list-to-string func-parameters)))
            (cond
                ((string= func-parameters-string "()") (concatenate 'string "(declaim (ftype (function () " func-return-type ") " func-name "))")) ;; if the function has no parameters
                (t
                    (let* ((func-parameters-clean (subseq func-parameters-string 1 (- (length func-parameters-string) 1))) ;; remove the paranthesis
                            (func-parameters-list (split-string "," func-parameters-clean 0 '() "" nil)) ;; split the parameters by comma (inta intb intc)
                            (lisp-type-function-parameters (declare-func-parameters func-parameters-list))) 
                        (cond
                            ((string= func-return-type "int") (concatenate 'string "(declaim (ftype (function (" (string-trim " " lisp-type-function-parameters) ") integer) " func-name "))"))
                            ((string= func-return-type "float") (concatenate 'string "(declaim (ftype (function ("(string-trim " " lisp-type-function-parameters) ") single-float) " func-name "))"))
                            ((string= func-return-type "double") (concatenate 'string "(declaim (ftype (function (" (string-trim " " lisp-type-function-parameters) ") double-float) " func-name "))"))
                            ((string= func-return-type "char") (concatenate 'string "(declaim (ftype (function (" (string-trim " " lisp-type-function-parameters) ") character) " func-name "))"))
                            ((string= func-return-type "void") (concatenate 'string "(declaim (ftype (function (" (string-trim " " lisp-type-function-parameters) ") void) " func-name "))")))))))
)



(defun convert-return-statement (line)
    ;; converts a return statement to a lisp return statement return <arithmetical-expr> <space> ;
    ;; subseq line 1 (- (length line) 1) removes the return keyword and the semicolon
    (let ((return-value (subseq line 1 (- (length line) 1))))
        (if return-value
            (concatenate 'string (evaluate-infix-arithmetic-expression return-value '() '() 0))
            "nil"
        )
    )
)

(defun convert-end-block (line)
    (concatenate 'string ")")
)

(defun empty-line (line)
    ""
)

(defun conversion-foo (type)
    ;; returns the proper conversion function for the given type
    (cond
        ((string= type "if-statement") #'convert-if-statement)
        ((string= type "for-loop") #'convert-for-loop)
        ((string= type "while-loop") #'convert-while-loop)
        ((string= type "variable-assignment") #'convert-variable-assignment)
        ((string= type "fuction-defination") #'convert-fuction-defination)
        ((string= type "fuction-declaration") #'convert-fuction-declaration)
        ((string= type "return-statement") #'convert-return-statement)
        ((string= type "variable-re-assignment") #'convert-variable-re-assignment)
        ((string= type "end-block") #'convert-end-block)
        ((string= type "empty-line") #'empty-line)
        (t (error "Unknown type"))
    )
)

(defun line-type (line)
    (cond
        ((< (length line) 1) "empty-line")                  ;; empty line case (new line or tab or space)
        ((string= (nth 0 line) "if") "if-statement")        ;; if the first word of the line is if
        ((string= (nth 0 line) "for") "for-loop")           ;; if the first word of the line is for
        ((string= (nth 0 line) "while") "while-loop")       ;; if the first word of the line is while
        ((and (> (length line) 2)                           ;; if the lines first word is a variable type and 3rd word is "=" [int a =]
                (is-variable-type (nth 0 line)) (string= (nth 2 line) "=")) "variable-assignment")
        ((and (> (length line) 2)                           ;; if the lines second word is "=" [a = 10 ;] 
                (string= (nth 1 line) "=")) "variable-re-assignment")
        ((and (> (length line) 2)                           ;; if the lines first word is a variable type and last word is "{" [int a () {]
                (is-variable-type (nth 0 line)) (string= (nth (- (length line) 1) line) "{")) "fuction-defination")
        ((and (> (length line) 2)                           ;; if the lines first word is a variable type and last word is ";" [int a () ;] 
                (is-variable-type (nth 0 line)) (string= (nth (- (length line) 1) line) ";")) "fuction-declaration")
        ((string= (nth 0 line) "return")                    ;; if the first word of the line is return
                "return-statement")
        ((string= (nth 0 line) "}") "end-block")            ;; if the first word of the line is }
    )
)

(defun write-file (lines)
    ;; writes the given lines to a file
    ;; :direction the type of the stream, :input for reading, :output for writing
    ;; if-exists :supersede to overwrite, :append to append
    (with-open-file (stream "output.lisp" :direction :output :if-exists :supersede)
        (loop for line in lines do
            (format stream "~a~%" line)
        )
    )
)

(defun convert (line conversion-func)
    (conversion-func line)
)

(defun read-file (fileName)
    ;; reads the file and returns a list of lines
    ;; :direction the type of the stream, :input for reading, :output for writing
    (with-open-file (stream fileName :direction :input)
        (loop for line = (read-line stream nil) ; read a line from the file
            while line ;; while there are lines to read
            collect line ;; collect is the feature of loop that collects the results of the loop
        )
    )
)

(defun main (lines index converted-lines)
    ;; main function that converts the given lines to lisp
    (if (< index (length lines))
        (let* ((line (split-string " " (line-by-index lines index) 0 '() "" nil))
               (type-of-line (line-type line))
               (conversion-func (conversion-foo type-of-line))
               (converted-line (funcall conversion-func line)))
            (main lines (+ index 1) (append converted-lines (list converted-line)))
        )
        converted-lines
    )
)

(write-file (main (read-file "main.c") 0 '()))