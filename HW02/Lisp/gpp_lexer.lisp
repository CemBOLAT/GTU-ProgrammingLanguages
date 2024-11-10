(setf keyword-list-and-value '(
    ("and" "KW_AND")
    ("or" "KW_OR")
    ("not" "KW_NOT")
    ("equal" "KW_EQUAL")
    ("less" "KW_LESS")
    ("nil" "KW_NIL")
    ("list" "KW_LIST")
    ("append" "KW_APPEND")
    ("concat" "KW_CONCAT")
    ("set" "KW_SET")
    ("deffun" "KW_DEFFUN")
    ("for" "KW_FOR")
    ("if" "KW_IF")
    ("exit" "KW_EXIT")
    ("load" "KW_LOAD")
    ("print" "KW_DISP")
    ("true" "KW_TRUE")
    ("false" "KW_FALSE")
))

(setf operator-list-and-value '(
    ("+" "OP_PLUS")
    ("-" "OP_MINUS")
    ("/" "OP_DIV")
    ("*" "OP_MULT")
    ("(" "OP_OP")
    (")" "OP_CP")
    ("," "OP_COMMA")
))

(setf comments '(
    (";;" "COMMENT")
))

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
        (is-string-in-list-helper string list 0)
    )
)

(defun is-keyword (token)
    (let ((keywords (mapcar #'car keyword-list-and-value))
            (values (mapcar #'cdr keyword-list-and-value)))
        (multiple-value-bind (is-in-list index) (is-string-in-list token keywords)
            (if is-in-list
                (nth 0 (nth index values))
                nil
            )
        )
    )
)

(defun is-operator (token)
    (let ((operators (mapcar #'car operator-list-and-value))
            (values (mapcar #'cdr operator-list-and-value)))
        (multiple-value-bind (is-in-list index) (is-string-in-list token operators)
            (if is-in-list
                (nth 0 (nth index values))
                nil
            )
        )
    )
)

(defun is-identifier (token)
    ; Any combination of alphabetical characters, digits and “_” with only leading alphabetical characters.
    ;; lead with alphabetical characters
    (let ((first-char (char token 0)))
        (if (or (<= 65 (char-code first-char) 90) (<= 97 (char-code first-char) 122))
            (let ((len (length token)))
                (labels ((is-identifier-helper (index)
                            (if (>= index len)
                                t
                                (let ((current-char (char token index)))
                                    (if (or (<= 65 (char-code current-char) 90)     ;; A-Z
                                            (<= 97 (char-code current-char) 122)    ;; a-z
                                            (<= 48 (char-code current-char) 57)     ;; 0-9
                                            (string= current-char "_"))
                                        (is-identifier-helper (+ index 1))
                                        nil
                                    )
                                )
                            )
                        ))
                    (is-identifier-helper 1)
                )
            )
            nil
        )
    )
)

(defun is-valuef (token)
    ;; Check if the token is a valid unsigned integer or a fraction (numerator:denominator)
    (labels ((check-unsigned-integer (token index)
            ;; Helper function to check if the token is a valid unsigned integer
            (let ((len (length token)))
                (if (>= index len)
                   t
                   (let ((current-char (char token index)))
                        (if (<= 48 (char-code current-char) 57)    ;; 0-9
                            (check-unsigned-integer token (+ index 1))
                            nil
                        )
                    )
                ))
            ))
        (let ((colon-index (position (code-char 58) token)))  ;; :
            (if colon-index
                ;; If colon is found, check if both parts (numerator and denominator) are valid unsigned integers
                (let ((numerator (subseq token 0 colon-index))
                    (denominator (subseq token (+ colon-index 1))))
                    (and (check-unsigned-integer numerator 0)
                        (check-unsigned-integer denominator 0)))
                ;; If no colon, check if the entire token is a valid unsigned integer
                (check-unsigned-integer token 0)
            )
        )
    )
)



(defun is-valid-argument (args)
    (if (<= (length args) 1)
        t
        nil
    )
)

(defun split-line (line)
    ;; split line into whitespaces
    (let ((whitespaces '(#\Space #\Tab #\Newline)))
        (labels ((split-line-helper (line index len tokens token)
                    (if (>= index len)
                        (if (not (string= token ""))
                            (append tokens (list token))
                            tokens
                        )
                        (let ((char (char line index)))
                            (if (member char whitespaces)
                                (if (not (string= token ""))
                                    (split-line-helper line (+ index 1) len (append tokens (list token)) "")
                                    (split-line-helper line (+ index 1) len tokens "")
                                )
                                (split-line-helper line (+ index 1) len tokens (concatenate 'string token (string char)))
                            )
                        )
                    )
                ))
            (split-line-helper line 0 (length line) '() "")
        )
    )
)

(defun catogorize-token (token)
    (if (not (string= token ""))
        (let ((keyword (is-keyword token))
                (operator (is-operator token))
                (identifier (is-identifier token))
                (valuef (is-valuef token))
            )
            (cond
                (keyword keyword)
                (operator operator)
                (identifier "IDENTIFIER")
                (valuef "VALUEF")
                (t "INVALID")
            )
        )
    )
)

(defun start-state (token)
    (labels ((start-state-helper (token-on-hand start-index end-index)
                ;; Base case: when all characters have been processed
                (if (>= start-index end-index)
                    ;; If there is a token-on-hand, categorize and return it
                    (if (not (string= token-on-hand ""))
                        (list (catogorize-token token-on-hand)) 
                        nil)
                    ;; Process the current character
                    (let ((current-char (subseq token start-index (+ start-index 1)))
                            (next-char (if (< (+ start-index 1) end-index) (subseq token (+ start-index 1) (+ start-index 2)) "")))
                        (cond
                            ;; If current char is '(', handle OP-OP
                            ((string= current-char "(")
                            (append 
                                (if (string= token-on-hand "")
                                    '("OP-OP")
                                    (list (catogorize-token token-on-hand) "OP-OP")
                            )
                            (start-state-helper "" (+ start-index 1) end-index)))
                            ;; If current char is ')', handle OP-CP
                            ((string= current-char ")")
                            (append 
                                (if (string= token-on-hand "")
                                    '("OP-CP")
                                    (list (catogorize-token token-on-hand) "OP-CP")
                                )
                            (start-state-helper "" (+ start-index 1) end-index)))
                            ;; If current char is ',', handle OP-COMMA
                            ((string= current-char ",")
                                (append 
                                    (if (string= token-on-hand "")
                                        '("OP-COMMA")
                                        (list (catogorize-token token-on-hand) "OP-COMMA")
                                    )
                                (start-state-helper "" (+ start-index 1) end-index)
                                )
                            )
                            ;; If current char is ';', handle COMMENT
                            ((and (string= current-char ";") (string= next-char ";"))
                            (append
                                (if (string= token-on-hand "")
                                    '("COMMENT")
                                    (list (catogorize-token token-on-hand) "COMMENT")
                                )
                                nil
                            ))
                            ;; Default case: accumulate characters in token-on-hand
                            (t
                                (start-state-helper 
                                (concatenate 'string token-on-hand current-char) 
                                (+ start-index 1) end-index))
                        )
                    )
                )
            ))
        ;; Start the recursive helper function
        (start-state-helper "" 0 (length token))
    )
)


(defun lex-the-line (line)
    (let ((tokens (split-line line)))
        (labels ((lex-helper (index result)
                    (if (>= index (length tokens))
                        result
                        (let ((tokenized-line (start-state (nth index tokens))))
                            ;; if tokenized line has COMMENT, ignore the rest of the 
                            (format t "tokenized-line:~a:~%" tokenized-line)
                            (if (is-string-in-list "COMMENT" tokenized-line)
                                (append result tokenized-line)
                                (lex-helper (+ index 1) (append result tokenized-line))
                            )
                        )
                    )
            ))
            (lex-helper 0 '())
        )
    )
)

(defun print-list (list)
    (if list
        (progn
            (format t "~a~%" (car list))
            (print-list (cdr list))
        )
    )
)

(defun load-file (file-name)
    (if file-name
        (with-open-file (stream file-name :direction :input :if-does-not-exist nil)
            (if (not stream)
                (error "File not found: ~a" file-name))
                (labels ((read-file-helper (accumulated-tokens)
                        (let ((line (read-line stream nil)))
                            (if line
                                (let ((line-tokens (lex-the-line line)))
                                ;; Tüm token'ları topluyoruz
                                (read-file-helper (append accumulated-tokens line-tokens)))
                                ;; Tüm token'lar toplandıktan sonra, print-list çağrısı yapılır
                                (print-list accumulated-tokens)
                            )
                        )))
                (read-file-helper nil)
            )
        )
    )
)


(defun repl-process ()
    ; read eval print loop for lexer
    (let ((line (read-line)))
        (if line
            (progn
                (print-list (lex-the-line line))
                (repl-process)
            )
        )
    )
)

(defun gppinterpreter ()
    (let ((args *args*))
        (if (is-valid-argument args)
            (progn
                (load-file (nth 0 args))
                (repl-process)
            )
            (format t "Invalid arguments~%")
        )

    )
)


(gppinterpreter)