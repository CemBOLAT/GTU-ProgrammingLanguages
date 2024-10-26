;; Product Rules:
;; <line> ::= <if-statement> | <for-loop> | <while-loop> | <variable-assignment> | <fuction-definition> | <fuction-declaration> | <return-statement> | <variable-re-assignment> | <end-block> | <empty-line>
;; <if-statement> ::= if <space> ( <space> <logical-expression> <space> ) <space> { <line>* }
;; <for-loop> ::= for <space> ( <space> <variable-assignment> <space> ; <space> <logical-expression> <space> ; <space> <arithmetic-expr> <space> ) <space> { <line>* }
;; <while-loop> ::= while <space> ( <space> <logical-expression> <space> ) <space> { <line>* }
;; <variable-assignment> ::= <data-type> <space> <param-name> <space> = <space> <arithmetic-expr> <space> ; | <data-type> <space> <param-name> <space> = <space> <func-name> <space> (<function-call-params>) <space> ;
;; <fuction-definition> ::= <data-type> <space> <func-name> <space> (<function-decleration-params>) <space> { <line>* }
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

(defun list-to-string (list index separator)
    ;; converts a list to a string by concating element with one space
    (if (>= index (length list))
        ""
        (string-trim " " (concatenate 'string (nth index list) separator (list-to-string list (+ index 1) separator)))
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

(defun remove-whitespace (str)
    ;; removes the whitespaces from the given string
    (remove-if #'(lambda (char)
                 (find char " "))
             str)
)

(defun read-file-helper (stream index line-index)
    ;; reads the given file and returns the lines as a string
    (let ((line (read-line stream nil)))
        (if line
            (if (= index line-index)
                line
                (read-file-helper stream (+ index 1) line-index)
            )
            nil
        )
    )
)

(defun read-file (fileName index)
    ;; reads the given file and returns the lines as a list
    ;; :direction the type of the stream, :input for reading, :output for writing
    (with-open-file (stream fileName :direction :input)
        (read-file-helper stream 0 index)
    )
)

(defun write-file-helper (stream lines)
    ;; writes the given lines to a file
    (if lines
        (progn
            (format stream "~a~%" (car lines))
            (write-file-helper stream (cdr lines))
        )
        nil
    )
)

(defun write-file (lines)
    ;; writes the given lines to a file
    ;; :direction the type of the stream, :input for reading, :output for writing
    ;; if-exists :supersede to overwrite, :append to append
    (with-open-file (stream "output.lisp" :direction :output :if-exists :supersede)
        (write-file-helper stream lines)
    )
)

(defun line-by-index (lines index)
    ;; returns the line at the given index
    (if (< index (length lines))
        (nth index lines)
        (error "Index out of bounds")
    )
)

(defun is-start-with-data-type (line)
    ;; checks if the given line starts with a c-type data type
    (cond
        ((and (> (length line) 3) (string= (subseq line 0 3) "int")) t)
        ((and (> (length line) 4) (string= (subseq line 0 4) "char")) t)
        ((and (> (length line) 5) (string= (subseq line 0 5) "float")) t)
        ((and (> (length line) 6) (string= (subseq line 0 6) "double")) t)
        ((and (> (length line) 4) (string= (subseq line 0 4) "void")) t)
        (t nil)
    )
)

(defun convert-if-statement (line)
    ;; line example: "if (a < 10) {"
    ;; first remove unnecessary whitespaces and if + paranthesis
    ;; then add space before and after the logical delimeters
    ;; then split the line by ' ' to get the parts
    ;; then evaluate the infix logical expression
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (logical-expression (subseq trimmed-line 3 (- (length trimmed-line) 1)))
            (splitted-logical-expression (split-string " " (add-space-before-after-logical-delimeters logical-expression 0) 0 '() "" nil)))
        (concatenate 'string "(if " (evaluate-infix-logical-expression (reverse splitted-logical-expression) '() '() 0) " " '(#\Newline) "(progn")
    )
)

(defun convert-while-loop (line)
    ;; line example: "while (a < 10) {"
    ;; first remove unnecessary whitespaces and while + paranthesis
    ;; then add space before and after the logical delimeters
    ;; then split the line by ' ' to get the parts
    ;; then evaluate the infix logical expression
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (logical-expression (subseq trimmed-line 6 (- (length trimmed-line) 1)))
            (splitted-logical-expression (split-string " " (add-space-before-after-logical-delimeters logical-expression 0) 0 '() "" nil)))
        (concatenate 'string "(loop while " (evaluate-infix-logical-expression (reverse splitted-logical-expression) '() '() 0) " do" '(#\Newline) "(progn")
    )
)

(defun extract-var-name (variable-name)
    ;; extracts the variable name from the given variable name
    ;; example: int a --> a
    (cond
        ((and (> (length variable-name) 3) (string= (subseq variable-name 0 3) "int")) (subseq variable-name 3 (length variable-name)))
        ((and (> (length variable-name) 4) (string= (subseq variable-name 0 3) "char")) (subseq variable-name 4 (length variable-name)))
        ((and (> (length variable-name) 4) (string= (subseq variable-name 0 3) "void")) (subseq variable-name 4 (length variable-name)))
        ((and (> (length variable-name) 5) (string= (subseq variable-name 0 3) "float")) (subseq variable-name 5 (length variable-name)))
        ((and (> (length variable-name) 6) (string= (subseq variable-name 0 3) "double")) (subseq variable-name 6 (length variable-name)))
    )
)


(defun extract-end-value (expr varname relational-operator)
    ;; example: a < 5 / a > 5 / a <= 5 / a >= 5
    ;; expr is a string that contains the relational operator and the value
    ;; extract the value from the expression 
    (let* ((split-parts (split-string relational-operator expr 0 nil "" nil)))
        (if (string= (nth 0 split-parts) varname)
            (nth 1 split-parts)
            (nth 0 split-parts)
        )
    )
)

(defun extract-increment-value (expr varname)
    ;; example: a++ / a-- / a + 5 / a - 5
    ;; expr is a string that contains the increment value
    (cond
        ((search "++" expr) "1")
        ((search "--" expr) "-1")
        ((search "+" expr) (progn
                                (let* ((split-parts (split-string "+" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
        ((search "-" expr) (progn
                                (let* ((split-parts (split-string "-" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
    )
)

(defun extract-relational-operator (expr)
    ;; expr is a string that contains the relational operator and the variable name
    ;; we need to find which relational operator is used in the expression
    (cond
        ((search ">=" expr) ">=")
        ((search "<=" expr) "<=")
        ((search ">" expr) ">")
        ((search "<" expr) "<")
    )
)

(defun convert-for-loop (line)
    ;; line example: "for (int a = 0; a < 10; a++) {"
    ;; first remove unnecessary whitespaces and for + paranthesis
    ;; then split the line by ';' to get the parts
    ;; then extract the variable name, start value, end value, relational operator and increment value
    ;; then return the lisp equivalent of the for loop
    (let* ((temp-line (remove-whitespace line))
            (trimmed-line (subseq temp-line 3 (- (length temp-line) 1)))
            (parantesis-deleted-line (subseq trimmed-line 1 (- (length trimmed-line) 1)))
            (splitted-line (split-string ";" parantesis-deleted-line 0 '() "" nil))
            (first-expr (subseq (nth 0 splitted-line) 0 (position #\= (nth 0 splitted-line))))
            (variable-name (extract-var-name first-expr))
            (start-value (subseq (nth 0 splitted-line) (+ 1 (position #\= (nth 0 splitted-line)))))
            (relational-operator (extract-relational-operator (nth 1 splitted-line)))
            (end-value (extract-end-value (nth 1 splitted-line) variable-name relational-operator))
            (increment-value (extract-increment-value (nth 2 splitted-line) variable-name))
        )
        (cond
            ((string= relational-operator "<") (concatenate 'string "(loop for " variable-name " from " start-value " below " end-value " by " increment-value " do" '(#\Newline) "(progn"))
            ((string= relational-operator ">") (concatenate 'string "(loop for " variable-name " from " start-value " downto " "( + " end-value " 1)" " by " increment-value " do" '(#\Newline) "(progn"))
            ((string= relational-operator ">=") (concatenate 'string "(loop for " variable-name " from " start-value " downto " end-value " by " increment-value " do" '(#\Newline) "(progn"))
            ((string= relational-operator "<=") (concatenate 'string "(loop for " variable-name " from " start-value " to " end-value " by " increment-value " do" '(#\Newline) "(progn"))
        )
    )
)

(defun precedence (operator)
    ;; returns the precedence of the given operator
    (cond
        ((string= operator "+") 1)
        ((string= operator "-") 1)
        ((string= operator "*") 2)
        ((string= operator "/") 2)
        ((string= operator "%") 2)
        ((string= operator "||") 0)
        ((string= operator "&&") 1)
        ((or (string= operator "==") (string= operator "!=") (string= operator "<") (string= operator ">")
            (string= operator "<=") (string= operator ">=")) 2)
        ((string= operator "!") 3)
        (t -1)))

(defun is-operator (operator)
    ;; checks if the given operator is an operator
    (cond
        ((string= operator "+") t)
        ((string= operator "-") t)
        ((string= operator "*") t)
        ((string= operator "/") t)
        ((string= operator "%") t)
        ((string= operator "||") t)
        ((string= operator "&&") t)
        ((string= operator "==") t)
        ((string= operator "!=") t)
        ((string= operator "<") t)
        ((string= operator ">") t)
        ((string= operator "<=") t)
        ((string= operator ">=") t)
        ((string= operator "!") t)
        (t nil)
    )
)

(defun logical-operator-to-lisp (operator)
    ;; converts the given logical operator to lisp equivalent
    (cond
        ((string= operator "&&") "and")
        ((string= operator "||") "or")
        ((string= operator "!") "not")
        ((string= operator "==") "eq")
        ((string= operator "!=") "/=")
        (t operator)
    )
)

(defun evaluate-infix-arithmetic-expression (infix operator-stack output-queue index)
    (if (>= index (length infix))
        (let* ((op (pop operator-stack)) 
                    (operand1 (pop output-queue)) (operand2 (pop output-queue)))
            (if (null op)
                (progn
                    (push (concatenate 'string operand1) output-queue)
                    (list-to-string output-queue 0 "")
                )
                (progn
                    (push (concatenate 'string "(" op " " operand1 " " operand2 ")") output-queue)
                    (if (null operator-stack)
                        (list-to-string output-queue 0 "")
                        (evaluate-infix-arithmetic-expression infix operator-stack output-queue (+ index 1))
                    )
                )
            )
        )
        (let* ((current-char (nth index infix)))
            (cond
                ((string= current-char ")") (push current-char operator-stack))
                ((string= current-char "(") (loop for i from 0 to (- (length operator-stack) 1) do
                                                (let ((operator (pop operator-stack)))
                                                    (if (string= operator ")")
                                                        (return)
                                                        (let* ((operand1 (pop output-queue)) (operand2 (pop output-queue)))
                                                            (push (concatenate 'string "(" operator " " operand1 " " operand2 ")") output-queue)
                                                        )
                                                    )
                                                    (evaluate-infix-arithmetic-expression infix operator-stack output-queue (+ index 1))
                                                )
                                            ))
                ((is-operator current-char) (if (null operator-stack)
                                                (push current-char operator-stack)
                                                (let ((top-operator (car operator-stack)))
                                                    (if (or (null top-operator) (string= top-operator "(") (>= (precedence current-char) (precedence top-operator)))
                                                        (push current-char operator-stack)
                                                        (let* ((op (pop operator-stack)) 
                                                                (operand1 (pop output-queue)) (operand2 (pop output-queue)))
                                                            (push (concatenate 'string "(" op " " operand1 " " operand2 ")") output-queue)
                                                            (push current-char operator-stack)
                                                        )
                                                    )
                                                )
                                            ))
                (t (push current-char output-queue))
            )
            (evaluate-infix-arithmetic-expression infix operator-stack output-queue (+ index 1))
        )
    )
)

(defun evaluate-infix-logical-expression (infix operator-stack output-queue index)
    (if (>= index (length infix))
        (let* ((op (pop operator-stack)) 
                    (operand1 (pop output-queue)) (operand2 (pop output-queue)))
            (if (null op)
                (progn
                    (push (concatenate 'string operand1) output-queue)
                    (list-to-string output-queue 0 "")
                )
                (progn
                    (push (concatenate 'string "(" (logical-operator-to-lisp op) " " operand1 " " operand2 ")") output-queue)
                    (if (null operator-stack)
                        (list-to-string output-queue 0 "")
                        (evaluate-infix-logical-expression infix operator-stack output-queue (+ index 1))
                    )
                )
            )
        )
        (let* ((current-char (nth index infix)))
            (cond
                ((string= current-char ")") (push current-char operator-stack))
                ((string= current-char "(") (loop for i from 0 to (- (length operator-stack) 1) do
                                                (let ((operator (pop operator-stack)))
                                                    (if (string= operator ")")
                                                        (return)
                                                        (let* ((operand1 (pop output-queue)) (operand2 (pop output-queue)))
                                                            (push (concatenate 'string "(" (logical-operator-to-lisp operator) " " operand1 " " operand2 ")") output-queue)
                                                        )
                                                    )
                                                    (evaluate-infix-logical-expression infix operator-stack output-queue (+ index 1))
                                                )
                                            ))
                ((is-operator current-char) (if (null operator-stack)
                                                (push current-char operator-stack)
                                                (let ((top-operator (car operator-stack)))
                                                    (if (or (null top-operator) (string= top-operator "(") (>= (precedence current-char) (precedence top-operator)))
                                                        (push current-char operator-stack)
                                                        (let* ((op (pop operator-stack)) 
                                                                (operand1 (pop output-queue)) (operand2 (pop output-queue)))
                                                            (push (concatenate 'string "(" (logical-operator-to-lisp op) " " operand1 " " operand2 ")") output-queue)
                                                            (push current-char operator-stack)
                                                        )
                                                    )
                                                )
                                            ))
                (t (push current-char output-queue))
            )
            (evaluate-infix-logical-expression infix operator-stack output-queue (+ index 1))
        )
    )
)

(defun convert-variable-re-assignment (line)
    ;; line example: "a = 5;"
    ;; first remove unnecessary whitespaces
    ;; then variable name is the first word
    ;;      value is the words from the first '=' to the end except the ';'
    ;;      if the value is a function call then split the parameters by ',' and convert them to lisp data types
    ;;      if the value is a function call without parameters then just return the function call
    ;;      if the value is an arithmetic expression then split the expression by ' ' and evaluate the expression
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
                (param-name (subseq trimmed-line 0 (position #\= trimmed-line)))
                (arithmetic-expr (remove-whitespace (subseq trimmed-line (+ 1 (position #\= trimmed-line)) (- (length trimmed-line) 1)))))
            (if (search "()" arithmetic-expr)
                (let ((func-name (subseq arithmetic-expr 0 (- (length arithmetic-expr) 2))))
                    (concatenate 'string "(setq " (string-trim " " param-name) " (" (string-trim " " func-name) "))")
                )
                (if (or (search "(" arithmetic-expr) (search "," arithmetic-expr))
                    (let* ((func-name (subseq arithmetic-expr 0 (position (code-char 40) arithmetic-expr)))
                            (func-params (subseq arithmetic-expr (+ 1 (position (code-char 40) arithmetic-expr)) (- (length arithmetic-expr) 1))))
                            (concatenate 'string "(setq " (string-trim " " param-name) " (" (string-trim " " func-name) " " 
                                        (list-to-string (split-string "," func-params 0 '() "" nil) 0 " ") "))")
                    )
                    (concatenate 'string "(setq " (string-trim " " param-name) " " (evaluate-infix-arithmetic-expression 
                            (reverse (split-string " " (add-space-before-after-delimeters arithmetic-expr 0) 0 '() "" nil)) '() '() 0) ")")
                
                )
            )
    )
)

(defun convert-variable-assignment (line)
    ;; line example: "int a = 5;"
    ;; first remove unnecessary whitespaces
    ;; then data type is the first word
    ;;      variable name is the second word till the first '='
    ;;      value is the words from the first '=' to the end except the ';'
    ;;      if the value is a function call then split the parameters by ',' and convert them to lisp data types
    ;;      if the value is a function call without parameters then just return the function call
    ;;      if the value is an arithmetic expression then split the expression by ' ' and evaluate the expression
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (data-type (subseq trimmed-line 0 (position #\space trimmed-line)))
            (line-without-data-type (subseq trimmed-line (+ 1 (position #\space trimmed-line))))
            (param-name (subseq line-without-data-type 0 (position #\= line-without-data-type)))
            (arithmetic-expr (remove-whitespace (subseq line-without-data-type (+ 1 (position #\= line-without-data-type)) (- (length line-without-data-type) 1)))))
            (if (search "()" arithmetic-expr)
                (let ((func-name (subseq arithmetic-expr 0 (- (length arithmetic-expr) 2))))
                    (concatenate 'string "(" (string-trim " " param-name) " (" (string-trim " " func-name) "))")
                )
                (if (or (search "(" arithmetic-expr) (search "," arithmetic-expr))
                    (let* ((func-name (subseq arithmetic-expr 0 (position (code-char 40) arithmetic-expr)))
                            (func-params (subseq arithmetic-expr (+ 1 (position (code-char 40) arithmetic-expr)) (- (length arithmetic-expr) 1))))
                            (concatenate 'string "(" (string-trim " " param-name) " (" (string-trim " " func-name) " " 
                                        (list-to-string (split-string "," func-params 0 '() "" nil) 0 " ") "))")
                    )
                    (concatenate 'string "(" (string-trim " " param-name) " " (evaluate-infix-arithmetic-expression 
                            (reverse (split-string " " (add-space-before-after-delimeters arithmetic-expr 0) 0 '() "" nil)) '() '() 0) ")")
                
                )
            )
    )
)

(defun defition-func-parameters (parameters)
    ;; takes a list of parameters and returns the string except the data type
    ;; (int a, int b) --> "a b"
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

(defun convert-fuction-definition (line)
    ;; line example: "int func1(int a, int b) {"
    ;; first remove unnecessary whitespaces
    ;; then data type is the first word
    ;;      function name is the second word till the first paranthesis
    ;;      parameters are the words from the first paranthesis to the end except the '{'
    ;; if there is no parameter then just return the function definition
    ;; if there is parameters then split the parameters by ',' and convert them to lisp data types
    (let* ((temp-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (trimmed-line (string-trim " " (subseq temp-line 0 (- (length temp-line) 1))))
            (func-return-type (subseq trimmed-line 0 (position #\space trimmed-line)))
            (func-name (subseq trimmed-line (+ 1 (position #\space trimmed-line)) (position (code-char 40) trimmed-line)))
            (parameters (subseq trimmed-line (+ 1 (position (code-char 40) trimmed-line)) (- (length trimmed-line) 1)))
        )
            (cond 
                ((or (string= parameters "") (string= parameters " ")) 
                    (concatenate 'string "(defun " (string-trim " " func-name) " () " '(#\Newline) "(let* ("))
                (t
                    (let* ((splitted-parameters (split-string "," (remove-whitespace parameters) 0 '() "" nil)))
                        (concatenate 'string "(defun " (string-trim " " func-name) " (" (string-trim " " (defition-func-parameters splitted-parameters)) ") " '(#\Newline) "(let* (")
                    )))))



(defun c-to-lisp-data-type (data-type)
    ;; converts a string c-type data type to lisp-type data type
    (cond
        ((string= data-type "int") "integer")
        ((string= data-type "float") "single-float")
        ((string= data-type "double") "double-float")
        ((string= data-type "char") "character")
        ((string= data-type "void") "nil")
    )
)

(defun convert-parameters-c-to-lisp (parameters)
    ;; converts a list of c-type parameters to string of lisp-type parameters
    (if (null parameters)
        ""
        (cond
            ((string= (subseq (car parameters) 0 3) "int") (concatenate 'string "integer " (convert-parameters-c-to-lisp (cdr parameters))))
            ((string= (subseq (car parameters) 0 4) "char") (concatenate 'string "character " (convert-parameters-c-to-lisp (cdr parameters))))
            ((string= (subseq (car parameters) 0 4) "void") (concatenate 'string "void " (convert-parameters-c-to-lisp (cdr parameters))))
            ((string= (subseq (car parameters) 0 5) "float") (concatenate 'string "single-float " (convert-parameters-c-to-lisp (cdr parameters))))
            ((string= (subseq (car parameters) 0 6) "double") (concatenate 'string "double-float " (convert-parameters-c-to-lisp (cdr parameters)))))))


(defun convert-fuction-declaration (line)
    ;; line example: "int func1(int a, int b);"
    ;; first remove unnecessary whitespaces
    ;; then data type is the first word
    ;;      function name is the second word till the first paranthesis
    ;;      parameters are the words from the first paranthesis to the end except the ';'
    ;; if there is no parameter then just return the function declaration
    ;; if there is parameters then split the parameters by ',' and convert them to lisp data types
    (let*
        ((temp-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
        (trimmed-line (string-trim " " (subseq temp-line 0 (- (length temp-line) 1))))
        (data-type (subseq trimmed-line 0 (position #\space trimmed-line)))
        (func-name (subseq trimmed-line (+ 1 (position #\space trimmed-line)) (position (code-char 40) trimmed-line)))
        (parameters (subseq trimmed-line (+ 1 (position (code-char 40) trimmed-line)) (- (length trimmed-line) 1))))
        (cond
            ((or (string= parameters "") (string= parameters " ")) 
                (concatenate 'string "(declaim (ftype (function () " (c-to-lisp-data-type data-type) ") " (string-trim " " func-name) "))"))
            (t
                (let* ((splitted-parameters (split-string "," (remove-whitespace parameters) 0 '() "" nil)))
                    (concatenate 'string "(declaim (ftype (function (" (string-trim " " (convert-parameters-c-to-lisp splitted-parameters)) ") " (c-to-lisp-data-type data-type) ") " (string-trim " " func-name) "))"))
            ))))

(defun add-space-before-after-delimeters (line index)
    ;; adds space before and after the delimeters
    (if (>= index (length line))
        ""
        (let* ((current-char (subseq line index (+ index 1))))
            (if (or (string= current-char "(") (string= current-char ")") 
                    (string= current-char "+") (string= current-char "-") (string= current-char "*") 
                    (string= current-char "/") (string= current-char "%"))
                (concatenate 'string " " current-char " " (add-space-before-after-delimeters line (+ index 1)))
                (concatenate 'string current-char (add-space-before-after-delimeters line (+ index 1)))))))

(defun add-space-before-after-logical-delimeters (line index)
    ;; adds space before and after the logical delimeters
    ;; first gets the current char and the next char
    ;; then checks if the two char is a logical delimeter
    ;; if it is a logical delimeter then adds space before and after the delimeter
    (if (>= index (length line))
        ""
            (let* ((current-char (subseq line index (+ index 1)))
                (next-char (if (< (+ index 1) (length line)) (subseq line (+ index 1) (+ index 2)) nil))
                (two-char-op (if next-char (concatenate 'string current-char next-char) nil)))
            (cond
                ((or (string= two-char-op "<=") (string= two-char-op ">=") (string= two-char-op "==") 
                    (string= two-char-op "!=") (string= two-char-op "&&") (string= two-char-op "||"))
                        (concatenate 'string " " two-char-op " " (add-space-before-after-logical-delimeters line (+ index 2))))
                ((or (string= current-char "<") (string= current-char ">") (string= current-char "!") 
                    (string= current-char "(") (string= current-char ")"))
                        (concatenate 'string " " current-char " " (add-space-before-after-logical-delimeters line (+ index 1))))
                (t (concatenate 'string current-char (add-space-before-after-logical-delimeters line (+ index 1))))))))


(defun convert-return-statement (line)
    ;; first remove the return keyword and the semicolon
    ;; then add space before and after the delimeters then split the string to get tokens
    ;; then evaluate the infix arithmetic expression
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (return-value (subseq trimmed-line 6 (- (length trimmed-line) 1)))
            (splitted-line (split-string " " (add-space-before-after-delimeters return-value 0) 0 '() "" nil)))
        (if splitted-line
            (concatenate 'string (evaluate-infix-arithmetic-expression (reverse splitted-line) '() '() 0))
            "nil"
        )
    )
)

(defun get-string-between-quotes (line)
    (let* ((quote-start (position #\" line))
            (quote-end (position #\" line :start (+ quote-start 1))))
        (if (and quote-start quote-end)
            (subseq line (+ quote-start 1) quote-end)
            "")))

(defun extract-formatters (line index)
    ;; extracts the new line character from the given line
    ;; if the new line character is not found then returns the line
    (if (>= index (length line))
        ""
        (let* ((current-char (subseq line index (+ index 1)))
                (next-char (if (< (+ index 1) (length line)) 
                                (subseq line (+ index 1) (+ index 2)) nil)))
            (cond
                ((and (string= current-char "\\") (string= next-char "n")) 
                    (concatenate 'string "~%" (extract-formatters line (+ index 2))))
                ((and (string= current-char "%") (string= next-char "d")) 
                    (concatenate 'string "~a" (extract-formatters line (+ index 2))))
                ((and (string= current-char "%") (string= next-char "f")) 
                    (concatenate 'string "~a" (extract-formatters line (+ index 2))))
                ((and (string= current-char "%") (string= next-char "c")) 
                    (concatenate 'string "~a" (extract-formatters line (+ index 2))))
                (t (concatenate 'string current-char (extract-formatters line (+ index 1))))
            )
        )
    )
)

(defun convert-print-statement (line)
    ;; example input : "printf("Hello World %d\n", x);"
    ;; example output : "(format t "Hello World ~a~%" x)"
    ;; Between the double quotes, the string is written as it is.
    ;; The variables in printf is refered by %and the type of the variable.
    ;; first remove the printf keyword and the semicolon
    ;; than get the string between the double quotes
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (print-value (subseq trimmed-line 7 (- (length trimmed-line) 2)))
            (str-between-quotes (get-string-between-quotes print-value))
            (new-line-converted (extract-formatters str-between-quotes 0))
            (parameters (split-string "," (subseq print-value (+ (length str-between-quotes) 2)) 0 '() "" nil)))
        (if (null parameters)
            (concatenate 'string "(format t \"" new-line-converted "\")")
            (concatenate 'string "(format t \"" new-line-converted "\" " (list-to-string parameters 0 " ") ")")
        )
    )
)

(defun convert-end-block (line)
    ;; if the line is end-block it returns the closing paranthesis
    ;; one for the progn and other for the scopes
    (concatenate 'string "))"))

(defun empty-line (line)
    "" )


(defun conversion-foo (type)
    ;; returns the proper conversion function for the given type
    (cond
        ((string= type "if-statement") #'convert-if-statement)
        ((string= type "for-loop") #'convert-for-loop)
        ((string= type "while-loop") #'convert-while-loop)
        ((string= type "variable-assignment") #'convert-variable-assignment)
        ((string= type "fuction-definition") #'convert-fuction-definition)
        ((string= type "fuction-declaration") #'convert-fuction-declaration)
        ((string= type "return-statement") #'convert-return-statement)
        ((string= type "print-statement") #'convert-print-statement)
        ((string= type "variable-re-assignment") #'convert-variable-re-assignment)
        ((string= type "end-block") #'convert-end-block)
        ((string= type "empty-line") #'empty-line)
        (t (error "Unknown type"))))


(defun line-type (line)
    ;; returns the type of the given line according to the production rules
    (let* ((line-without-space (remove-whitespace line)) (line-len (length line-without-space)))
        (cond
            ((string= line-without-space "") "empty-line")                  ;; empty line
            ((and (< 4 line-len) 
                    (string= (subseq line-without-space 0 2) "if")          ;; if the line starts with if and ends with {
                    (string= (subseq line-without-space (- line-len 1) line-len) "{")) "if-statement")
            ((and (< 5 line-len) 
                    (string= (subseq line-without-space 0 3) "for")         ;; if the line starts with for and ends with {
                    (string= (subseq line-without-space (- line-len 1) line-len) "{")) "for-loop")
            ((and (< 6 line-len) 
                    (string= (subseq line-without-space 0 5) "while")       ;; if the line starts with while and ends with {
                    (string= (subseq line-without-space (- line-len 1) line-len) "{")) "while-loop")
            ((and (= 1 line-len)                                            ;; if the line is }
                    (string= line-without-space "}") "end-block"))
            ((and (< 6 line-len)                                            ;; if the line starts with return
                (string= (subseq line-without-space 0 6) "return") "return-statement"))
            ((and (< 6 line-len)                                            ;; if the line starts with printf
                (string= (subseq line-without-space 0 6) "printf") "print-statement"))
            ((and (is-start-with-data-type line-without-space)              ;; if the line starts with a c-type data type and has a '=' sign
                (search "=" line-without-space) "variable-assignment"))
            ((and (is-start-with-data-type line-without-space)              ;; if the line starts with a c-type data type and ends with '{'
                (string= (subseq line-without-space (- line-len 1) line-len) "{") "fuction-definition"))
            ((and (is-start-with-data-type line-without-space)              ;; if the line starts with a c-type data type and ends with ';'
                    (string= (subseq line-without-space (- line-len 1) line-len) ";") "fuction-declaration"))
            ((and (search "=" line-without-space) "variable-re-assignment")) ;; if the line has a '=' sign
        )))


(defun add_till_space (line original_line index)
    ;; add till first character that is not a space
    (if (>= index (length original_line))
        ""
        (let* ((current-char (subseq original_line index (+ index 1))))
            (if (string= current-char " ")
                (concatenate 'string current-char (add_till_space line original_line (+ index 1)))
                line
            )
        )
    )
)
    

(defun main (converted-lines index)
    (let* ((next-line (read-file "main.c" index))
            (line-before (read-file "main.c" (- index 1))))
        (cond 
            ((null next-line) (write-file (reverse converted-lines)))
            ((null line-before) (progn
                                    (let* ((type-of-line (line-type next-line))
                                            (conversion-func (conversion-foo type-of-line))
                                            (converted-line (add_till_space (funcall conversion-func next-line) next-line 0)))
                                        (main (cons converted-line converted-lines) (+ index 1)))))
            (t
                (let* ((type-of-line (line-type next-line))
                        (type-of-line-before (line-type line-before)))
                        (if (or (and (string= type-of-line-before "variable-assignment") (not (string= type-of-line "variable-assignment")))
                                (and (string= type-of-line-before "fuction-definition") (not (string= type-of-line "variable-assignment"))))
                            ; kapalı parantez ekle converted-line listesine
                            (let* ((converted-once (cons (code-char 41) converted-lines))
                                    (conversion-func (conversion-foo type-of-line))
                                    (converted-line (add_till_space (funcall conversion-func next-line) next-line 0)))
                                (main (cons converted-line converted-once) (+ index 1))   
                            )
                            (let* ((conversion-func (conversion-foo type-of-line))
                                    (converted-line (add_till_space (funcall conversion-func next-line) next-line 0)))
                                (main (cons converted-line converted-lines) (+ index 1)))
                        )
                )
            )
            
        )
        ;;(if (null next-line)
        ;;    (write-file (reverse converted-lines))
        ;;    (let* ((type-of-line (line-type next-line))
        ;;            (conversion-func (conversion-foo type-of-line))
        ;;            (converted-line (funcall conversion-func next-line)))
        ;;        (main (cons converted-line converted-lines) (+ index 1))
        ;;    )
        ;;)
    )
)

(main '() 0)