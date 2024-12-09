;; DFA States and Transitions
;; 1. Initial state: (q0)
    ;; Read a letter (a-zA-Z): transition to q1 (potential identifier or keyword)
    ;; Read a digit (0-9): transition to q2 (potential integer)
    ;; Read '+ - / * ( ) ,': transition to the respective operator state (final state for operators) (q5)
    ;; Read ; : transition to q3 /comment state
    ;; Read ':': transition to q4 (potential fraction)
    ;; Read whitespace: stay in q0
;; 2. Identifier or keyword state (q1)
    ;; Read a letter (a-zA-Z_) or digit (0-9): stay in q1
    ;; Read whitespace: generate IDENTIFIER or KEYWORD token and transition to q0
;; 3. VALUEF or VALUEI state (q2)
    ;; Read a digit (0-9): stay in q2
    ;; Read ':' or 'f': transition to VALUEF state (q4)
    ;; Read whitespace: generate VALUEI token and transition to q0
;; 3. Comment state (q3)
    ;; if the next char is ';', stay in q3 and ignore the rest of the line
    ;; if the next char is not ';' Syntax error
;; 4. VALUEF state (q4)
    ;; Read a digit (0-9): stay in q4
    ;; Read whitespace: generate VALUEF token and transition to q0
;; 5. Operator state (q5)
    ;; Read next character: if it is '(' ')' or whitespace, generate the operator token and transition to q0
    ;; Otherwise, syntax error


;; START           → INPUT
;; INPUT           → COMMENT | EXPLIST
;; EXP             → OP_OP OP_PLUS EXP EXP OP_CP
;;                 | OP_OP OP_MINUS EXP EXP OP_CP
;;                 | OP_OP OP_MULT EXP EXP OP_CP
;;                 | OP_OP OP_DIV EXP EXP OP_CP
;;                 | VALUEF
;;                 | VALUEI
;;                 | IDENTIFIER
;;                 | SET
;;                 | OP_OP KW_IF EXPB EXPLIST EXPLIST OP_CP
;;                 | OP_OP KW_IF EXPB EXPLIST OP_CP
;;                 | OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXPLIST OP_CP
;;                 | OP_OP KW_LOAD IDENTIFIER OP_CP
;;                 | OP_OP KW_DEFFUN IDENTIFIER OP_OP PARAMLIST OP_CP EXPLIST OP_CP
;;                 | EXPB
;;                 | OP_OP KW_EXIT OP_CP
;;                 | OP_OP KW_PRINT EXP OP_CP
;;                 | LIST_INPUT
;;                 | FCALL
;; EXPB            → OP_OP KW_EQUAL EXP EXP OP_CP
;;                 | OP_OP KW_LESS EXP EXP OP_CP
;;                 | OP_OP KW_AND EXPB EXPB OP_CP
;;                 | OP_OP KW_OR EXPB EXPB OP_CP
;;                 | OP_OP KW_NOT EXPB OP_CP
;;                 | KW_TRUE
;;                 | KW_FALSE
;;                 | OP_OP KW_NIL OP_CP
;; EXPLIST         → EXP | EXPLIST EXP
;; LIST_INPUT      → OP_OP KW_APPEND LIST LIST OP_CP
;;                 | OP_OP KW_LIST VALUES OP_CP
;;                 | OP_OP KW_CONCAT LIST LIST OP_CP
;; LIST            → OP_APOSTROPHE OP_OP VALUES OP_CP
;;                 | OP_APOSTROPHE OP_OP OP_CP
;;                 | OP_OP KW_LIST VALUES OP_CP
;;                 | KW_NIL
;; VALUES          → VALUES VALUEF | VALUES VALUEI | VALUEF | VALUEI
;; FCALL           → OP_OP IDENTIFIER OP_CP
;;                 | OP_OP IDENTIFIER EXP OP_CP
;;                 | OP_OP IDENTIFIER EXP EXP OP_CP
;;                 | OP_OP IDENTIFIER EXP EXP EXP OP_CP
;; PARAMLIST       → IDENTIFIER | IDENTIFIER IDENTIFIER | IDENTIFIER IDENTIFIER IDENTIFIER
;; SET             → OP_OP KW_SET IDENTIFIER EXP OP_CP
;;                 | OP_OP KW_DEFVAR IDENTIFIER EXP OP_CP

(setf keyword-list-and-value '(
    ("and" "KW_AND") ("or" "KW_OR") ("not" "KW_NOT") ("equal" "KW_EQUAL")
    ("less" "KW_LESS") ("nil" "KW_NIL") ("list" "KW_LIST") ("append" "KW_APPEND")
    ("concat" "KW_CONCAT") ("set" "KW_SET") ("deffun" "KW_DEFFUN") ("for" "KW_FOR")
    ("if" "KW_IF") ("exit" "KW_EXIT") ("load" "KW_LOAD") ("print" "KW_DISP")
    ("true" "KW_TRUE") ("false" "KW_FALSE") ("defvar" "KW_DEFVAR")
))

(setf operator-list-and-value '(
    ("+" "OP_PLUS") ("-" "OP_MINUS") ("/" "OP_DIV") ("*" "OP_MULT") ("(" "OP_OP") (")" "OP_CP") ("," "OP_COMMA") ("'" "OP_APOSTROPHE")
))

(defvar *terminal-list* '(
    "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_COMMA" "OP_APOSTROPHE"
    "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND"
    "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP"
    "KW_TRUE" "KW_FALSE" "KW_DEFVAR" "IDENTIFIER" "VALUEF" "VALUEI" "COMMENT"
))

(defun is-letter (char)
    ;; Check if the character is a letter (a-zA-Z)
    (or (and (>= (char-code char) 65) (<= (char-code char) 90))
        (and (>= (char-code char) 97) (<= (char-code char) 122)))
)

(defun is-white-space (char)
    ;; Check if the character is a whitespace (space, tab, newline)
    (or (char= char #\Space)
        (char= char #\Tab)
        (char= char #\Newline))
)

(defun is-operator (char)
    ;; Check if the character is an operator (+ - / * ( ) ,)
    (or (= (char-code char) 43)  ; +
        (= (char-code char) 45)  ; -
        (= (char-code char) 47)  ; /
        (= (char-code char) 42)  ; *
        (= (char-code char) 40)  ; (
        (= (char-code char) 41)  ; )
        (= (char-code char) 44)) ; ,
)

(defun is-digit (char)
    ;; Check if the character is a digit (0-9)
    (and (>= (char-code char) 48) (<= (char-code char) 57)))


(defun print-list (list)
    ;; Print the list of tokens Recursively
    (if list
        (progn
            (format t "~a~%" (car list))
            (print-list (cdr list)))))

(defun is-valid-argument (args)
    ;; Check if the number of arguments is valid 
    (if (<= (length args) 1)
        t
        nil))

(defun is-string-in-list (string list)
    ;; Check if the string is in the list and return the index
    (labels ((is-string-in-list-helper (string list index)
                (if (>= index (length list))
                    (values nil nil)
                    (if (string= string (nth index list))
                        (values t index)
                        (is-string-in-list-helper string list (+ index 1))
                    )
                )
            ))
        (is-string-in-list-helper string list 0)))

(defun operator-value (char)
    ;; Get the value of the operator from the operator list
    (let ((operators (mapcar #'car operator-list-and-value))
            (values (mapcar #'cdr operator-list-and-value)))
        (multiple-value-bind (is-in-list index) (is-string-in-list (string char) operators)
            (if is-in-list
                (nth 0 (nth index values))
                (error "Syntax error: Invalid operator: ~a" char)))))

(defun identifier-or-keyword (token)
    ;; Get the value of the identifier or keyword from the keyword list
    (let ((keywords (mapcar #'car keyword-list-and-value))
            (values (mapcar #'cdr keyword-list-and-value)))
        (multiple-value-bind (is-in-list index) (is-string-in-list (string-downcase token) keywords)
            (if is-in-list
                (nth 0 (nth index values))
                "IDENTIFIER" ))))

(defun state-0 (line index)
    ;; Initial state: (q0)
    ;; Read a letter (a-zA-Z): transition to q1 (potential identifier or keyword)
    ;; Read a digit (0-9): transition to q2 (potential integer)
    ;; Read '+ - / * ( ) ,': transition to the respective operator state (final state for operators) (q5)
    ;; Read ';' : transition to q3 /comment state
    ;; Read ':': transition to q4 (potential fraction)
    ;; Read whitespace: stay in q0
    (let ((char (char line index)))
        (cond
            ((is-letter char) (state-1 line (+ index 1) (string char)))
            ((is-digit char) (state-2 line (+ index 1) (string char)))
            ((is-operator char) (state-5 line (+ index 1) char))
            ((char= char #\:) (state-4 line (+ index 1) (string char)))
            ((char= char #\;) (state-3 line (+ index 1)))
            ((is-white-space char) (values (+ index 1) nil nil))
            (t (error "Syntax error: Invalid character: ~a" char)))))

(defun state-1 (line index token)
    ;; Go till the end of the identifier or keyword with the following rules:
    ;; 1. If the next character is a letter, digit or '_', stay in state-1
    ;; 2. If the next character is whitespace, generate the token and transition to state-0
    ;; 3. If the next character is '(' or ')', generate the token and transition to state-0
    ;; 4. Otherwise, syntax error
    ;; Returns new index and the token
    (let ((line-length (length line)))
        (labels ((state-1-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (or (is-letter char) (is-digit char) (= (char-code char) 95)) ; letter, digit or '_'
                        (state-1-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (is-white-space char) ; whitespace
                            (values (+ index 1) (list (identifier-or-keyword token) token ))
                            (if (or (= (char-code char) 40) (= (char-code char) 41)) ; ( )
                                (values index (list (identifier-or-keyword token) token))
                                (error "Syntax error: ~a cannot be followed by ~a" token char)
                            )
                        )
                    )
                )
                (values index (list (identifier-or-keyword token) token)) ; end of the line
            )
        ))
        (state-1-helper line index token)))) ; start the helper function

(defun state-2 (line index token)
    ;; Go till the end of the integer with the following rules:
    ;; 1. If the next character is a digit, stay in state-2
    ;; 2. If the next character is ':' or 'f', transition to state-4 (potential fraction)
    ;; 3. If the next character is whitespace, generate the token and transition to state-0
    ;; 4. If the next character is '(' or ')', generate the token and transition to state-0
    (let ((line-length (length line)))
        (labels ((state-2-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (is-digit char) ; digit
                        (state-2-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (or (char= char #\:) (char= char #\f)) ; : or f
                            (state-4 line (+ index 1) (concatenate 'string token (string char)))
                            (if (is-white-space char) ; whitespace
                                (values (+ index 1) (list "VALUEI" token))
                                (if (or (= (char-code char) 40) (= (char-code char) 41)) ; ( )
                                    (values index (list "VALUEI" token))
                                    (error "Syntax error: ~a cannot token be followed by ~a" token char))))))
                (values index (list "VALUEI" token)) ; end of the line
            )))
        (state-2-helper line index token))))

(defun state-3 (line index)
    ;; if the next char is ';' stay in state-3 and ignore the rest of the line
    ;; if the next char is not ';' Syntax error
    (let ((line-length (length line)))
        (if (< index line-length)
            (let ((char (char line index)))
                (if (char= char #\;)
                    (values line-length (list "COMMENT" ";;") ) ; end of the line
                    (error "Syntax error: Invalid character: ~a" char))) ; syntax error
            (error "Syntax error: Missing ';' at the end of the line")))) ; syntax error

(defun state-4 (line index token)
    ;; Go till the end of the fraction with the following rules:
    ;; 1. If the next character is a digit, stay in state-4
    ;; 2. If the next character is whitespace, generate the token and transition to state-0
    ;; 3. If the next character is '(' or ')', generate the token and transition to state-0
    ;; 4. Otherwise, syntax error
    (let ((line-length (length line)))
        (labels ((state-4-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (is-digit char)
                        (state-4-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (is-white-space char)
                            (values (+ index 1) (list "VALUEF" token))
                            (if (or (= (char-code char) 40) (= (char-code char) 41)) ; ( )
                                (values index (list "VALUEF" token))
                                (error "Syntax error: ~a cannot be followed by ~a" token char)))))
                (values index (list "VALUEF" token)))))
        (state-4-helper line index token))))

(defun state-5 (line index token)
    ;; State for operators
    ;; Check the next char - if it is not '(' ')' or whitespace, syntax error
    ;; Returns the operator token and transitions to state-0
    (if (or (= (char-code token) 40) (= (char-code token) 41))
        (values index (list (operator-value (string token))))
        (if (< index (length line))
            (let ((next-char (char line index)))
                (if (or (= (char-code next-char) 40) (= (char-code next-char) 41) (is-white-space next-char))
                    (values index (list (operator-value (string token)) token))
                    (error "Syntax error: ~a cannot be followed by ~a" token next-char)))
            (values index (list (operator-value (string token)) token)))))

(defun dfa (line)
    ;; DFA for the lexer that returns the list of tokens
    (let ((line-length (length line)))
        (labels ((dfa-helper (line index list)
            (if (< index line-length)
                (multiple-value-bind (new-index token) (state-0 line index) ; calls the initial states and saves values to new-index and token
                    (if token ; if token is not nil (not whitespace)
                        (dfa-helper line new-index (append list (list token))) ; append the token to the list
                        (dfa-helper line new-index list)
                    ))
                list)))
        (dfa-helper line 0 nil))))

(defun print-test-list (list)
    ;; Print the list of tokens Recursively
    (if list
        (progn
            (let ((token (car list)))
                (format t "token: ~a, value: ~a~%" (car token) (cadr token))
            )
            (print-test-list (cdr list))
        )
    )

)

(defun repl-process ()
    ;; read eval print loop for lexer
    (format t "> ")
    (let ((line (read-line)))
        (if line
            (if (string= (string-trim " " line) "quit") ; exit the interpreter
                (format t "Exiting the interpreter~%")
                (progn
                    (format t "answer-of-shift-reduce-parser: ~a~%" (top-down-parser (dfa line)))
                    ;(print-test-list (dfa line))
                    (repl-process))))))

(defun load-file (file-name)
    ;; read the file and print the tokens
    ;; if the file is not found, print an error
    (if file-name
        (with-open-file (stream file-name :direction :input :if-does-not-exist nil)
            (if (not stream)
                (error "File not found: ~a" file-name))
                (labels ((read-file-helper (accumulated-tokens)
                        (let ((line (read-line stream nil)))
                            (if line
                                (let ((line-tokens (dfa line)))
                                    (read-file-helper (append accumulated-tokens line-tokens))) ;; read the next line
                                (print-list accumulated-tokens))))) ;; end of the file
                (read-file-helper nil)))))

(defun gppinterpreter ()
    ;; main function for the interpreter
    (let ((args *args*)) ;; get the arguments
        (if (is-valid-argument args)
            (progn
                (setf id-temp 0)
                (setf value-temp 0)
                (format t "Print \"quit\" to exit the interpreter~%")
                (load-file (nth 0 args)) ;; load the file
                (repl-process)
            )
            (format t "Invalid number of arguments~%"))))


(gppinterpreter)