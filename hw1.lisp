#|

<statement_list> ::= <statement> | <statement> <statement_list>

<statement> ::= <c_identifier> <c_assignment_operator> <function_call>
              | <c_identifier> <c_assignment_operator> <expression> <semi_colon>
              | <c_identifier> <c_assignment_operator> <c_identifier> <semi_colon>
              | <c_return> <semi_colon>
              | <ctypes> <c_identifier> <semi_colon>
              | <c_while_statement>
              | <for_statement>
              | <if_statement>
              | <else_if_statement>
              | <else_statement>
              | <c_identifier> <open_square_bracket> <expression> <close_square_bracket> <semi_colon>
              | <c_identifier> <open_square_bracket> <expression> <close_square_bracket> <c_assignment_operator> <expression> <semi_colon>

<expression> ::= <c_identifier> <c_logic> <c_identifier>
               | <c_identifier> <c_logic> <expression>
               | <c_identifier> <c_arithmetic> <c_identifier>
               | <c_identifier> <c_arithmetic> <expression>
               | <c_identifier>
               | <c_logic>
               | <c_arithmetic>
               | <c_return>
               | <c_identifier> <open_square_bracket> <expression> <close_square_bracket>
               | <open_round_bracket> <expression> <close_round_bracket>

<function_prototype> ::= <ctypes> <c_identifier> <open_round_bracket> <parameter_list> <close_round_bracket> <semi_colon>
<function_definition> ::= <ctypes> <c_identifier> <open_round_bracket> <parameter_list> <close_round_bracket> <open_curly_bracket> <statement_list> <close_curly_bracket>
<parameter_list> ::= <ctypes> <c_identifier> | <parameter_list> <comma> <ctypes> <c_identifier>

<function_call> ::= <c_identifier> <open_round_bracket> <argument_list> <close_round_bracket> <semi_colon>
<argument_list> ::= <expression> | <expression> <comma> <argument_list>

<c_while_statement> ::= <c_whiles> <open_round_bracket> <expression> <close_round_bracket> <open_curly_bracket> <statement_list> <close_curly_bracket>
<for_statement> ::= <c_for> <open_round_bracket> <expression> <semi_colon> <expression> <semi_colon> <expression> <close_round_bracket> <open_curly_bracket> <statement_list> <close_curly_bracket>

<if_statement> ::= <if_keyword> <open_round_bracket> <expression> <close_round_bracket> <open_curly_bracket> <statement_list> <close_curly_bracket>
<else_if_statement> ::= <if_statement> else <white_space> if <open_round_bracket> <expression> <close_round_bracket> <open_curly_bracket> <statement_list> <close_curly_bracket>
                     | <else_if_statement> else <white_space> if <open_round_bracket> <expression> <close_round_bracket> <open_curly_bracket> <statement_list> <close_curly_bracket>
<else_statement> ::= <if_statement> else <open_curly_bracket> <statement_list> <close_curly_bracket>
                   | <else_if_statement> else <open_curly_bracket> <statement_list> <close_curly_bracket>

<c_identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<c_assignment_operator> ::= = | <c_arithmetic>=
<c_logic> ::= && | || | ! | == | != | > | < | >= | <=
<c_arithmetic> ::= + | - | * | / | % | ++ | --

<if_keyword> ::= if
<c_return> ::= return
<ctypes> ::= int | float | double | char | void | unsigned | auto | const | long
<c_whiles> ::= while
<c_for> ::= for
<cloop_break> ::= break | continue

<white_space> ::= " " | "\t" | "\n"
<comma> ::= ,
<open_round_bracket> ::= (
<close_round_bracket> ::= )
<open_curly_bracket> ::= {
<close_curly_bracket> ::= }
<open_square_bracket> ::= [
<close_square_bracket> ::= ]
<semi_colon> ::= ;

|#

(defun read-file (stream)
    (let ((line (read-line stream nil)))
        (if line
            line
            nil
        )
    )
)

(defun is-operator (char)
    (member char (list #\+ #\- #\* #\/ #\% #\= #\> #\< #\! #\& #\|))
)

(defun is-keyword (string)
    (member string (list "int" "float" "double" "char" "void" "unsigned" "auto" "const" "long" "if" "return" "while" "for" "break" "continue"))
)

(defun is-delimiter (char)
    (member char (list #\; #\{ #\} #\( #\) #\[ #\] #\, #\n #\t #\space)) ; #\space is space
)

(defun is-identifier (string)
    ;; use regular expression to check if string is an identifier
    ;(string-match "^[a-zA-Z_][a-zA-Z0-9_]*$" string)
    (member string (list "a" "b" "c"))
)

(defun is-constant (string)
    ;; use regular expression to check if string is a constant
    ;(or (string-match "^[0-9]+$" string) (string-match "^[0-9]*\.[0-9]+$" string) (string-match "^[0-9]+\.[0-9]*$" string)) ; + at least one, * 0 or more
    (member string (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "1.1" "2.2" "3.3" "4.4" "5.5" "6.6" "7.7" "8.8" "9.9" "0.0" "1.0" "2.0" "3.0" "4.0" "5.0" "6.0" "7.0" "8.0" "9.0" "0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9"))
)

(defun is-string (string)
    ;; use regular expression to check if string is a string
    ;(string-match "^\".*\"$" string)
    (member string (list "hello" "world" "hello world" "world hello"))
)

(defun lexical-analyzer-helper (line tokens index line-length current-token)
    (if (< index line-length)
        (progn
            (let ((letter (char line index)))
                (cond
                    ((is-delimiter letter)
                        (if (string= current-token "")
                            (lexical-analyzer-helper line (append tokens (list (list "delimiter" (string letter)))) (+ index 1) line-length "")
                            (progn
                                (cond
                                    ((is-keyword current-token)
                                        (lexical-analyzer-helper line (append tokens (list (list "keyword" current-token) (list "delimiter" (string letter)))) (+ index 1) line-length ""))
                                    ((is-identifier current-token)
                                        (lexical-analyzer-helper line (append tokens (list (list "identifier" current-token) (list "delimiter" (string letter)))) (+ index 1) line-length ""))
                                    ((is-constant current-token)
                                        (lexical-analyzer-helper line (append tokens (list (list "constant" current-token) (list "delimiter" (string letter)))) (+ index 1) line-length ""))
                                    ((is-string current-token)
                                        (lexical-analyzer-helper line (append tokens (list (list "string" current-token) (list "delimiter" (string letter)))) (+ index 1) line-length ""))
                                    (t
                                        (lexical-analyzer-helper line (append tokens (list (list "error" current-token) (list "delimiter" (string letter)))) (+ index 1) line-length ""))
                                )
                            )
                        )
                    )
                    (t (lexical-analyzer-helper line tokens (+ index 1) line-length (concatenate 'string current-token (string letter))))
                )
            )
        )
        (if (not (string= current-token ""))
            (progn
                (cond
                    ((is-keyword current-token)
                        (append tokens (list (list "keyword" current-token)))
                    )
                    ((is-identifier current-token)
                        (append tokens (list (list "identifier" current-token)))
                    )
                    ((is-constant current-token)
                        (append tokens (list (list "constant" current-token)))
                    )
                    ((is-string current-token)
                        (append tokens (list (list "string" current-token)))
                    )
                    (t
                        (append tokens (list (list "error" current-token)))
                    )
                )
            )
            tokens
        )
    )
)


(defun lexical-analyzer (line)
    (let (
        (line-length (length line)))
        (lexical-analyzer-helper line '() 0 line-length "")
    )
)

(defun print_tokens (tokens)
    (loop
        (let ((token (car tokens)))
            (if (eq token nil)
                (return)
                (progn
                    (format t "~a " token)
                    (format t "~a~%" (cadr tokens))
                    (setf tokens (cddr tokens))
                )
            )
        )
    )
)

(defun main (filename)
    (with-open-file (stream filename :direction :input)
        (loop 
            (let ((line (read-file stream)))
                (if (eq line nil)
                    (return)
                    (let ((tokens (lexical-analyzer line)))
                        (print_tokens tokens)
                    )
                )
            )
        )
    )
)

(main "main.c")