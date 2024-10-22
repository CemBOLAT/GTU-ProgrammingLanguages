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
  (remove-if #'(lambda (char)
                 (find char " "))
             str)
)

(defun read-file-helper (stream lines)
    (let ((line (read-line stream nil)))
        (if line
            (read-file-helper stream (cons line lines))
            (reverse lines)
        )
    )
)

(defun read-file (fileName)
    (with-open-file (stream fileName :direction :input)
        (read-file-helper stream '())
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

(defun line-by-index (lines index)
    ;; returns the line at the given index
    (if (< index (length lines))
        (nth index lines)
        (error "Index out of bounds")
    )
)

(defun is-start-with-data-type (line)
    (cond
        ((and (> (length line) 3) (string= (subseq line 0 3) "int")) t)
        ((and (> (length line) 4) (string= (subseq line 0 4) "char")) t)
        ((and (> (length line) 5) (string= (subseq line 0 5) "float")) t)
        ((and (> (length line) 6) (string= (subseq line 0 6) "double")) t)
        ((and (> (length line) 4) (string= (subseq line 0 4) "void")) t)
        (t nil)
    )
)

(defun line-type (line)
    (let* ((line-without-space (remove-whitespace line)) (line-len (length line-without-space)))
        (cond
            ((string= line-without-space "") "empty-line")
            ((and (< 4 line-len) 
                    (string= (subseq line-without-space 0 2) "if")
                    (string= (subseq line-without-space (- line-len 1) line-len) "{")) "if-statement")
            ((and (< 5 line-len) 
                    (string= (subseq line-without-space 0 3) "for")
                    (string= (subseq line-without-space (- line-len 1) line-len) "{")) "for-loop")
            ((and (< 6 line-len) 
                    (string= (subseq line-without-space 0 5) "while")
                    (string= (subseq line-without-space (- line-len 1) line-len) "{")) "while-loop")
            ((and (= 1 line-len)
                    (string= line-without-space "}") "end-block"))
            ((and (< 6 line-len)
                (string= (subseq line-without-space 0 6) "return") "return-statement"))
            ((and (is-start-with-data-type line-without-space) (search "=" line-without-space) "variable-assignment"))
            ((and (is-start-with-data-type line-without-space) (string= (subseq line-without-space (- line-len 1) line-len) "{") "fuction-defination"))
            ((and (is-start-with-data-type line-without-space) (string= (subseq line-without-space (- line-len 1) line-len) ";") "fuction-declaration"))
            ((and (search "=" line-without-space) "variable-re-assignment"))

        )
    )
)

(defun convert-if-statement (line)
    (format nil "~a~%" line)

)

(defun convert-for-loop (line)
    (format nil "~a~%" line)

)

(defun convert-while-loop (line)
    (format nil "~a~%" line)

)

(defun evaluate-function-call (expr-list index)
    (if (>= index (length expr-list))
        ""
        (string-trim " " (concatenate 'string (nth index expr-list) " " (evaluate-function-call expr-list (+ index 1))))
    )
)

;; öncelik kısmı eksik brom.
(defun evaluate-infix-arithmetic-expression (infix operator-stack output-queue index)
    (if (>= index (length infix))
        (let* ((op (pop operator-stack)) (operand1 (pop output-queue)) (operand2 (pop output-queue)))
            (if (null op)
                (progn
                    (push (concatenate 'string operand1) output-queue)
                    (list-to-string output-queue 0 "")
                )
                (progn
                    (push (concatenate 'string "(" op " " operand2 " " operand1 ")") output-queue)
                    (if (null operator-stack)
                        (list-to-string output-queue 0 "")
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

(defun convert-variable-assignment (line)
    ;; line: "int cemal = 2 + 2 + 2 * 2 ;"
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (data-type (subseq trimmed-line 0 (position #\space trimmed-line)))
            (line-without-data-type (subseq trimmed-line (+ 1 (position #\space trimmed-line))))
            (param-name (subseq line-without-data-type 0 (position #\= line-without-data-type)))
            (arithmetic-expr (subseq line-without-data-type (+ 1 (position #\= line-without-data-type)) (- (length line-without-data-type) 1))))
        (if (search "," arithmetic-expr)
                ;; funcName(param1, param2, param3) -->
                (let* ((string-no-space (remove-whitespace arithmetic-expr))
                        (func-name (subseq string-no-space 0 (position #\( string-no-space)))
                        (func-params (subseq string-no-space (+ 1 (position #\( string-no-space)) (- (length string-no-space) 1))))
                        (concatenate 'string "(setq " (string-trim " " param-name) " (" (string-trim " " func-name) " " (evaluate-function-call (split-string "," func-params 0 '() "" nil) 0) "))")
                )
            (if (search "()" arithmetic-expr)
                (let ((func-name (subseq arithmetic-expr 0 (- (length arithmetic-expr) 3))))
                    (concatenate 'string "(setq " (string-trim " " param-name) " (" (string-trim " " func-name) "))")
                )
                (concatenate 'string "(setq " (string-trim " " param-name) " " (evaluate-infix-arithmetic-expression (split-string " " (add-space-before-after-delimeters arithmetic-expr 0) 0 '() "" nil) '() '() 0) ")")
            )
        )
    )
)

(defun convert-fuction-defination (line)
    (format nil "~a~%" line)

)

(defun convert-fuction-declaration (line)
    (let*
        ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
        (data-type (subseq trimmed-line 0 (position #\space trimmed-line)))
        )
        (format t "data-type :~a: trimmed-line :~a:~%" data-type trimmed-line)
    )    
)

(defun add-space-before-after-delimeters (line index)
    (if (>= index (length line))
        ""
        (let ((current-char (subseq line index (+ index 1))))
            (if (or (string= current-char "(") (string= current-char ")") (string= current-char "+") (string= current-char "-") (string= current-char "*") (string= current-char "/"))
                (concatenate 'string " " current-char " " (add-space-before-after-delimeters line (+ index 1)))
                (concatenate 'string current-char (add-space-before-after-delimeters line (+ index 1)))
            )
        )
    )
)

(defun convert-return-statement (line)
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
            (return-value (subseq trimmed-line 6 (- (length trimmed-line) 1)))
            (splitted-line (split-string " " (add-space-before-after-delimeters return-value 0) 0 '() "" nil)))
        (if splitted-line
            (concatenate 'string (evaluate-infix-arithmetic-expression splitted-line '() '() 0))
            "nil"
        )
    
    )
)

(defun convert-variable-re-assignment (line)
    (let* ((trimmed-line (list-to-string (split-string " " line 0 '() "" nil) 0 " "))
                (param-name (subseq trimmed-line 0 (position #\= trimmed-line)))
                (arithmetic-expr (subseq trimmed-line (+ 1 (position #\= trimmed-line)) (- (length trimmed-line) 1))))
            (if (search "," arithmetic-expr)
                    ;; funcName(param1, param2, param3) -->
                    (let* ((string-no-space (remove-whitespace arithmetic-expr))
                            (func-name (subseq string-no-space 0 (position #\( string-no-space)))
                            (func-params (subseq string-no-space (+ 1 (position #\( string-no-space)) (- (length string-no-space) 1))))
                            (concatenate 'string "(setq " (string-trim " " param-name) " (" (string-trim " " func-name) " " (evaluate-function-call (split-string "," func-params 0 '() "" nil) 0) "))")
                    )
                (if (search "()" arithmetic-expr)
                    (let ((func-name (subseq arithmetic-expr 0 (- (length arithmetic-expr) 3))))
                        (concatenate 'string "(setq " (string-trim " " param-name) " (" (string-trim " " func-name) "))")
                    )
                    (concatenate 'string "(setq " (string-trim " " param-name) " " (evaluate-infix-arithmetic-expression (split-string " " (add-space-before-after-delimeters arithmetic-expr 0) 0 '() "" nil) '() '() 0) ")")
                )
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



(defun main (lines index converted-lines)
    (if (< index (length lines))
        (let* ((line (line-by-index lines index))
                (type-of-line (line-type line))
                (conversion-func (conversion-foo type-of-line))
                (converted-line (funcall conversion-func line)))
            (main lines (+ index 1) (append converted-lines (list converted-line)))
        )
        converted-lines
    )
)

(write-file (main (read-file "main.c") 0 '()))