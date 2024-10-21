;; #|

;; The primary conversion function must be recursive. Each line of C code
;; should be processed one at a time, and the conversion function should call itself recursively
;; until the end of the file is reached.

;; Min Requirements for C code:

;; 1. if statement : if (logical-expression) {statement}
;; 2. logical-and-aritmatic operation:
;;     2.1. aritmatic-expression: +, -, *, /, %, ++, --
;;     2.2. logical-expression: && || !
;;     2.3. relational-expression: <, >, <=, >=, ==, !=
;; 3. for and while loop:
;;     3.1, for loop: for (expr1 expr2 expr3) {statement} // dont forget semicolon
;;     3.2, while loop: while (logical-expression) {statement}
;; 4. variable  assignment and declaration
;;     4.1. variable assignment: int a = (expr) | a = (expr);
;;     4.2. variable declaration: int a;
;; 4. function and function prototype definitions
;;     4.1. function prototype: int func(int a, int b);
;;     4.2. function definition: int func(int a, int b) {statement}
;; 5. function calls
;;     5.1. function call: func(a, b);
;; 6. variable assignment by return
;;     6.1. return statement: return (expr);
;;     6.2. variable assignment by return: int a = func(a, b);

;; Product Rules:
;; <lines> ::= <line> <lines> | <line>
;; <line> ::= <if-statement> | <for-loop> | <while-loop> | <variable-declaration> | <variable-assignment> | <function-prototype> | <function-definition> | <function-call> | <return-statement> | <variable-assignment-by-return> | <aritmatic-expression> | <logical-expression> | <relational-expression>
;; <if-statement> ::= if ( <logical-expression> ) { <line> | <lines> }
;; ;<for-loop> ::= for ( <expr>; <expr>; <expr>; ) { <line> | <lines> }
;; <while-loop> ::= while ( <logical-expression> ) { <line> | <lines> }
;; <variable-declaration> ::= <variable-type> <variable-name>;
;; <variable-assignment> ::= <variable-type> <variable-name> = <expr>;
;; <function-prototype> ::= <variable-type> <function-name>(<variable-type> <variable-name>, <variable-type> <variable-name>, ...);
;; <function-definition> ::= <variable-type> <function-name>(<variable-type> <variable-name>, <variable-type> <variable-name>, ...) { <line> | <lines> }
;; <function-call> ::= <function-name>(<variable-name>, <variable-name>, ...)
;; <return-statement> ::= return <expr> | return | return (<expr>);
;; <variable-assignment-by-return> ::= <variable-type> <variable-name> = <function-name>(<variable-name>, <variable-name>, ...);
;; <aritmatic-expression> ::= <value> + <value> | <value> - <value> | <value> * <value> | <value> / <value> | <value> % <value> | <value>++ | <value>--
;; <logical-expression> ::= ( <relational expression> && <relational expression> ) | ( <relational expression> || <relational expression> ) | !<relational expression>
;; <relational-expression> ::= <value> < <value> | <value> > <value> | <value> <= <value> | <value> >= <value> | <value> == <value> | <value> != <value>
;; <value> ::= <variable-name> | <number> | <function-call> | <variable-assignment-by-return> | (<expr>)
;; <expr> ::= <value> | <aritmatic-expression> | <logical-expression> | <relational-expression> | <variable declaration> | <variable-assignment>
;; <variable-type> ::= int | float | double | char | void
;; <variable-name> ::= [a-zA-Z_][a-zA-Z0-9_]*
;; <function-name> ::= [a-zA-Z_][a-zA-Z0-9_]*
;; <number> ::= [0-9]+

;; |#


;; line-type: This function takes a line of C code as input and returns the type of the line
;; as one of the types listed above.

;; conversion-foo: This function takes the line type as input and returns the appropriate
;; conversion function for that type.

;; convert: This function takes two arguments: the line of C code and the conversion
;; function. It returns the corresponding Lisp code for that line by applying the conversion
;; function. You can use a counter parameter for line index if it is required in your reading
;; function.

;; read_file: A function that reads the input C file and returns the text content line by line.
;; You can use external libraries for reading the file. The function may be an iterator that
;; returns the next line at each call, or it can return the line by a given index.

;; write_file: A function that writes the fully converted Lisp code to the output file after all
;; conversions are complete.

;; |#

(defun get-next-word-index (line index)
    (let ((len (length line)))
        (if (>= index len)
            nil
            (let ((start index))
                (loop for i from index below len
                    do (if (char= (char line i) #\Space)
                        (loop for j from i below len
                            do (if (not (char= (char line j) #\Space))
                                (return j)
                            )
                        )
                    )
                    finally (return len)
                )
            )
        )
    )
)

(defun convert-expression (expression)
  
  (let* ((trimmed-expr (string-trim " " (subseq expression 1 (1- (length expression)))))
         (parts (split-string trimmed-expr)))
    (cond
      ;; Aritmetik işlemler
      ((string= (second parts) "+")
       (format nil "(+ ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) "-")
       (format nil "(- ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) "*")
       (format nil "(* ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) "/")
       (format nil "(/ ~a ~a)" (first parts) (third parts)))

      ;; Mantıksal işlemler
      ((string= (second parts) "&&")
       (format nil "(and ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) "||")
       (format nil "(or ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) "<")
       (format nil "(< ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) ">")
       (format nil "(> ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) "<=")
       (format nil "(<= ~a ~a)" (first parts) (third parts)))
      ((string= (second parts) ">=")
       (format nil "(>= ~a ~a)" (first parts) (third parts)))

      ;; Desteklenmeyen ifadeler
      (t (format nil "Unsupported expression: ~a" expression)))))



(defun split-string (string)
  (let ((start 0)
        (result '()))
    (loop for i from 0 below (length string)
          do (when (char= (char string i) #\Space)
               (when (< start i)
                 (push (subseq string start i) result))
               (setf start (1+ i)))
          finally (when (< start (length string))
                    (push (subseq string start (length string)) result)))
    (nreverse result)))

(defun is-start-with-variable-type (line)
    (let ((len (length line)))
        (and (>= len 6)
            (or (string= (subseq line 0 3) "int") (string= (subseq line 0 4) "char") (string= (subseq line 0 5) "float") (string= (subseq line 0 6) "double"))
        )
    )
)

(defun is-start-with-function-type (line)
    (let ((len (length line)))
        (and (>= len 6)
            (or (string= (subseq line 0 3) "int") (string= (subseq line 0 4) "char") (string= (subseq line 0 5) "float") (string= (subseq line 0 6) "double") (string= (subseq line 0 4) "void"))
        )
    ))

(defun read-file (fileName)
    (with-open-file (stream fileName :direction :input)
        (loop for line = (read-line stream nil)
            while line
            collect line
        )
    )
)

(defun is-return-statement (line)
    (and (>= (length line) 6)
        (string= (subseq line 0 6) "return")
    )
)

(defun line-by-index (lines index)
    (nth index lines)
)


(defun line-type (line)
    (cond
        ((= (length line) 0)
         "empty-line")
        ((and (>= (length line) 2)
         (string= (subseq line 0 2) "if") (string= (subseq line (- (length line) 1)) "{"))
            "if-statement")
        ((and (>= (length line) 5)
         (string= (subseq line 0 3) "for") (string= (subseq line (- (length line) 1)) "{"))
            "for-statement")
        ((and (>= (length line) 5)
         (string= (subseq line 0 5) "while") (string= (subseq line (- (length line) 1)) "{"))
            "while-statement")
        ((and (is-start-with-variable-type line) (search "=" line) (string= (subseq line (- (length line) 1)) ";"))
            "variable-declaration")
        ((and (is-start-with-function-type line) (search "(" line) (search ")" line) (string= (subseq line (- (length line) 1)) ";"))
            "function-prototype")
        ((and (is-start-with-function-type line) (string= (subseq line (- (length line) 1)) "{"))
            "function-definition")
        ((and (is-start-with-variable-type line) (search "=" line) (string= (subseq line (- (length line) 1)) ";"))
            "variable-assignment")
        ((and (search "(" line) (search ")" line) (string= (subseq line (- (length line) 1)) ";"))
            "function-call")
        ((is-return-statement line)
            "return-statement")
        ((string= line "}")
            "end-of-block")
        (t "unknown")
    )
)

(defun write-file (lines)
    (with-open-file (stream "output.lisp" :direction :output :if-exists :supersede)
        (loop for line in lines
            do (progn
                (format stream "~a~%" line)
                (format t "Line Length ~a Line: ~a~%" (length line) line)
            )
        )
    )
)

(defun if-conversion (line)
    ; if (logical-expression) { --> (if (logical-expression) -> it will closed by end-of-block-conversion
    (let ((logical-expression (subseq line 3 (- (length line) 2))))
        ; return logical-expression
        (format nil "(if ~a" logical-expression)
    )
)

(defun for-conversion (line)
    "For Statement"
)

(defun while-conversion (line)
    "While Statement"
)

(defun variable-declaration-conversion (line)
    "Variable Declaration"
)

(defun get-parameter-types (index line string)
    ;; buradaki her bir keliyeli seç ve o kelime int ise stringe integer ekle double/float ise stringe float ekle

    (let ((len (length line)))
    (if (>= index len)
        string
        (let ((start index))
            (loop for i from index below len
                do (if (char= (char line i) #\Space)
                    (let ((word (subseq line start i)))
                        (if (string= word "int")
                            (get-parameter-types (+ i 1) line (concatenate 'string string "integer "))
                            (if (or (string= word "double") (string= word "float"))
                                (get-parameter-types (+ i 1) line (concatenate 'string string "float "))
                                (get-parameter-types (+ i 1) line string)
                            )
                        )
                    )
                )
                finally (return string)
            )
        )
    )
    )

)

(defun function-prototype-conversion (line)
    ;  sum is function name -> int sum(int a, int b); (declaim (ftype (function (integer integer) integer) sum))
    ;; (let ((funct-type) (subseq line 0 (position #\( line))
    (let* ((var-type-index (get-next-word-index line 0)) 
            (funct-type (subseq line 0 var-type-index))
            (funct-name-index (get-next-word-index line var-type-index))
            (funct-name (subseq line var-type-index funct-name-index))
            (parameter-types (get-parameter-types funct-name-index line "")
            ))
            (format t "var-type-index: ~a, funct-type: ~a, funct-name-index: ~a, funct-name: ~a, parameter-types: ~a~%" var-type-index funct-type funct-name-index funct-name parameter-types)
            ;(format nil "(declaim (ftype (function (~a) ~a) ~a))" parameter-types funct-type funct-name)
    )
)

(defun function-definition-conversion (line)
    "Function Definition"
)

(defun variable-assignment-conversion (line)
    "Variable Assignment"
)

(defun function-call-conversion (line)
    "Function Call"
)

(defun return-statement-conversion (line)
    "Return Statement"
)

(defun end-of-block-conversion (line)
    (format nil ")")
)

(defun unknown-conversion (line)
    (format t "Unknown: ~a~%" line)
)




(defun conversion-foo (type-of-line)

    (cond
        ((string= type-of-line "if-statement")
            'if-conversion)
        ((string= type-of-line "for-statement")
            'for-conversion)
        ((string= type-of-line "while-statement")
            'while-conversion)
        ((string= type-of-line "variable-declaration")
            'variable-declaration-conversion)
        ((string= type-of-line "function-prototype")
            'function-prototype-conversion)
        ((string= type-of-line "function-definition")
            'function-definition-conversion)
        ((string= type-of-line "variable-assignment")
            'variable-assignment-conversion)
        ((string= type-of-line "function-call")
            'function-call-conversion)
        ((string= type-of-line "return-statement")
            'return-statement-conversion)
        ((string= type-of-line "end-of-block")
            'end-of-block-conversion)
        (t 'unknown-conversion)
    )
)

(defun convert (line conversion-function)
  (if conversion-function
      (funcall conversion-function line)
      (format t "No conversion function available for line: ~a~%" line)))

(defun main (lines index converted-lines)
  (if (< index (length lines))
      (let ((line (string-trim " " (line-by-index lines index))))
        (let ((type-of-line (line-type line)))
          (let ((conversion-function (conversion-foo type-of-line))) ; Corrected spelling
            (if conversion-function
                (let ((lisp-line (convert line conversion-function))) 
                  (main lines (+ index 1) (append converted-lines (list lisp-line)))
                  )
                (progn
                  (format t "Unknown line type for line: ~a~%" line)
                  (main lines (+ index 1) converted-lines)) ; Continue processing
              )
            )
          )
        )
      converted-lines
    )
)

(write-file (main (read-file "main.c") 0 '()))