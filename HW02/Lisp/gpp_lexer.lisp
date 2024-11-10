;; COMMENT [;;].*
;; VALUEI [0]|[1-9][0-9]*
;; VALUEF {VALUEI}[:f]{VALUEI}|{VALUEI}[:f]|[:f]{VALUEI}
;; IDENTIFIER  [a-zA-Z][a-zA-Z0-9_]*
;; OP_PLUS     [+]
;; OP_MINUS    [-]
;; OP_DIV      [/]
;; OP_MULT     [*]
;; OP_OP       [(]
;; OP_CP       [)]
;; OP_COMMA    [,]

;; and      KW_AND
;; or       KW_OR
;; not      KW_NOT
;; equal    KW_EQUAL
;; less     KW_LESS
;; nil      KW_NIL
;; list     KW_LIST
;; append   KW_APPEND
;; concat   KW_CONCAT
;; set      KW_SET
;; deffun   KW_DEFFUN
;; for      KW_FOR
;; if       KW_IF
;; exit     KW_EXIT
;; load     KW_LOAD
;; print    KW_DISP
;; true     KW_TRUE
;; false    KW_FALSE

;; Finite state automata for the lexer
;; The lexer is a deterministic finite automaton (DFA) that reads the input string from left to right and generates a sequence of tokens.
;; The lexer has a finite number of states and transitions between states.
;; The lexer reads the input string character by character and changes its state based on the current state and the character read.
;; The lexer generates a token when it reaches a final state.
;; The lexer can be implemented as a table-driven DFA.

;; DFA States and Transitions
;; 1. Initial state: (q0)
;; Read a letter (a-zA-Z): transition to q1 (potential identifier or keyword)
;; Read a digit (0-9): transition to q2 (potential integer)
;; Read '+ - / * ( ) ,': transition to the respective operator state (final state for operators) (q5)
;; Read ; : transition to q3 /comment state
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

(setf keyword-list-and-value '(
    ("and" "KW_AND") ("or" "KW_OR") ("not" "KW_NOT") ("equal" "KW_EQUAL")
    ("less" "KW_LESS") ("nil" "KW_NIL") ("list" "KW_LIST") ("append" "KW_APPEND")
    ("concat" "KW_CONCAT") ("set" "KW_SET") ("deffun" "KW_DEFFUN") ("for" "KW_FOR")
    ("if" "KW_IF") ("exit" "KW_EXIT") ("load" "KW_LOAD") ("print" "KW_DISP")
    ("true" "KW_TRUE") ("false" "KW_FALSE")
))

(setf operator-list-and-value '(
    ("+" "OP_PLUS") ("-" "OP_MINUS") ("/" "OP_DIV") ("*" "OP_MULT") ("(" "OP_OP") (")" "OP_CP") ("," "OP_COMMA")
))

(defun is-letter (char)
    "Checks if the character is a letter (a-zA-Z)."
    (or (and (>= (char-code char) 65) (<= (char-code char) 90))
        (and (>= (char-code char) 97) (<= (char-code char) 122)))
)

(defun is-white-space (char)
    "Checks if the character is a whitespace character (space, tab, newline)."
    (or (char= char #\Space)
        (char= char #\Tab)
        (char= char #\Newline))
)

(defun is-operator (char)
    "Checks if the character is an operator (+, -, *, /, (, ), ,)."
    (or (= (char-code char) 43)  ; +
        (= (char-code char) 45)  ; -
        (= (char-code char) 47)  ; /
        (= (char-code char) 42)  ; *
        (= (char-code char) 40)  ; (
        (= (char-code char) 41)  ; )
        (= (char-code char) 44)) ; ,
)

(defun is-digit (char)
    "Checks if the character is a digit (0-9)."
    (and (>= (char-code char) 48) (<= (char-code char) 57)))


(defun print-list (list)
    (if list
        (progn
            (format t "~a~%" (car list))
            (print-list (cdr list)))))

(defun is-valid-argument (args)
    (if (<= (length args) 1)
        t
        nil))

(defun is-string-in-list (string list)
    ;; use string= to compare strings
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
    (let ((operators (mapcar #'car operator-list-and-value))
            (values (mapcar #'cdr operator-list-and-value)))
        (multiple-value-bind (is-in-list index) (is-string-in-list (string char) operators)
            (if is-in-list
                (nth 0 (nth index values))
                (error "Syntax error: Invalid operator: ~a" char)))))

(defun identifier-or-keyword (token)
    (let ((keywords (mapcar #'car keyword-list-and-value))
            (values (mapcar #'cdr keyword-list-and-value)))
        (multiple-value-bind (is-in-list index) (is-string-in-list (string-downcase token) keywords)
            (if is-in-list
                (nth 0 (nth index values))
                "IDENTIFIER" ))))

(defun state-0 (line index)
    (let ((char (char line index)))
        (cond
            ((is-letter char) (state-1 line (+ index 1) (string char)))
            ((is-digit char) (state-2 line (+ index 1) (string char)))
            ((is-operator char) (values (+ index 1) (list (operator-value char))))
            ((char= char #\;) (state-3 line (+ index 1)))
            ((is-white-space char) (values (+ index 1) nil))
            (t (error "Syntax error: Invalid character: ~a" char)))))

(defun state-1 (line index token)
    ;; Go till whitespace or end of line use label.
    (let ((line-length (length line)))
        (labels ((state-1-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (or (is-letter char) (is-digit char) (= (char-code char) 95)) ; _
                        (state-1-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (is-white-space char)
                            (values (+ index 1) (list (identifier-or-keyword token)))
                            (if (or (= (char-code char) 40) (= (char-code char) 41)) ; ( )
                                (values (+ index 1) (list (identifier-or-keyword token) (operator-value char)))
                                (error "Syntax error: ~a cannot token be followed by ~a" token char)
                            )
                        )
                    )
                )
                (values index (list (identifier-or-keyword token)))
            )
        ))
        (state-1-helper line index token))))

(defun state-2 (line index token)
    (let ((line-length (length line)))
        (labels ((state-2-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (is-digit char)
                        (state-2-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (or (char= char #\:) (char= char #\f))
                            (state-4 line (+ index 1) (concatenate 'string token (string char)))
                            (if (is-white-space char)
                                (values (+ index 1) (list "VALUEI"))
                                (if (or (= (char-code char) 40) (= (char-code char) 41)) ; ( )
                                    (values (+ index 1) (list "VALUEI" (operator-value char)))
                                    (error "Syntax error: ~a cannot token be followed by ~a" token char)
                                )
                            )
                        )
                    )
                )
                (values index (list "VALUEI"))
            )
        ))
        (state-2-helper line index token))
    )
)

(defun state-3 (line index)
    ;; if the next char is ';' stay in state-3 and ignore the rest of the line
    ;; if the next char is not ';' Syntax error
    (let ((line-length (length line)))
        (if (< index line-length)
            (let ((char (char line index)))
                (if (char= char #\;)
                    (values line-length (list "COMMENT"))
                    (error "Syntax error: Invalid character: ~a" char)
                )
            )
            (error "Syntax error: Missing ';' at the end of the line")
        )
    )
)

(defun state-4 (line index token)
    (let ((line-length (length line)))
        (labels ((state-4-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (is-digit char)
                        (state-4-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (is-white-space char)
                            (values (+ index 1) (list "VALUEF"))
                            (if (or (= (char-code char) 40) (= (char-code char) 41)) ; ( )
                                (values (+ index 1) (list "VALUEF" (operator-value char)))
                                (error "Syntax error: ~a cannot token be followed by ~a" token char)
                            )
                        )
                    )
                )
                (values index (list "VALUEF"))
            )
        ))
        (state-4-helper line index token))
    )
)

(defun dfa (line)
    (let ((line-length (length line)))
        (labels ((dfa-helper (line index list)
            (if (< index line-length)
                (multiple-value-bind (new-index token) (state-0 line index)
                    (if token
                        (dfa-helper line new-index (append list token))
                        (dfa-helper line new-index list)
                    )
                )
                list
            )
        ))
        (dfa-helper line 0 nil))))

(defun repl-process ()
    ; read eval print loop for lexer
    (let ((line (read-line)))
        (if line
            (progn
                (print-list (dfa line))
                (repl-process)))))

(defun load-file (file-name)
    (if file-name
        (with-open-file (stream file-name :direction :input :if-does-not-exist nil)
            (if (not stream)
                (error "File not found: ~a" file-name))
                (labels ((read-file-helper (accumulated-tokens)
                        (let ((line (read-line stream nil)))
                            (if line
                                (let ((line-tokens (dfa line)))
                                    (read-file-helper (append accumulated-tokens line-tokens)))
                                (print-list accumulated-tokens)
                            )
                        )))
                (read-file-helper nil)))))

(defun gppinterpreter ()
    (let ((args *args*))
        (if (is-valid-argument args)
            (progn
                (load-file (nth 0 args))
                (repl-process)
            )
            (format t "Invalid number of arguments~%"))))


(gppinterpreter)