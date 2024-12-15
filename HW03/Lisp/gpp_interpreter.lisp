(setf keyword-list-and-value '(
    ("and" "KW_AND") ("or" "KW_OR") ("not" "KW_NOT") ("equal" "KW_EQUAL")
    ("less" "KW_LESS") ("nil" "KW_NIL") ("list" "KW_LIST") ("append" "KW_APPEND")
    ("concat" "KW_CONCAT") ("set" "KW_SET") ("deffun" "KW_DEFFUN") ("for" "KW_FOR")
    ("if" "KW_IF") ("exit" "KW_EXIT") ("load" "KW_LOAD") ("print" "KW_PRINT")
    ("true" "KW_TRUE") ("false" "KW_FALSE") ("defvar" "KW_DEFVAR") ("while" "KW_WHILE")
))

(setf operator-list-and-value '(
    ("+" "OP_PLUS") ("-" "OP_MINUS")
    ("/" "OP_DIV") ("*" "OP_MULT") ("(" "OP_OP") (")" "OP_CP")
    ("," "OP_COMMA") ("'" "OP_APOSTROPHE")
))

(defun parse-aritmetic (tokens operator is-binay)
    ;; Parse the add operation and return the result
    ;; OP_OP OP_PLUS EXP EXP OP_CP

    (let ((first-token (car tokens)))
        (if (and (string= first-token "OP_OP") (string= (car (cdr tokens)) operator))
            (let* ((exp1 (parse-exp (cddr tokens)))
                  (exp2 (if is-binay (parse-exp exp1) exp1))
                  (next-tokens exp2)
                )
                (if (and (not (null exp1)) (not (null exp2)))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_CP"))
                            (if (cdr next-tokens)
                                (cdr next-tokens)
                                t
                            )
                            nil )
                        nil )
                    nil ))
            nil )))

(defun parse-if-else (tokens is-if-else)
    ;; Parse the if operation and return the result
    ;; OP_OP KW_IF EXPB EXP EXP OP_CP
    ;; OP_OP KW_IF EXPB EXP OP_CP
    
    (let ((first-token (car tokens)))
        (if (and (string= first-token "OP_OP") (string= (car (cdr tokens)) "KW_IF"))
            (let* ((expb (parse-expb (cddr tokens)))
                  (exp1 (parse-exp expb))
                  (exp2 (if is-if-else (parse-exp exp1) exp1))
                  (next-tokens exp2))
                (if (and (not (null expb)) (not (null exp1)) (not (null exp2)))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_CP"))
                            (if (cdr next-tokens)
                                (cdr next-tokens)
                                t)
                            nil)
                        nil)
                    nil))
            nil)))


(defun parse-values (tokens value-type)
    ;; Parse the values and return the result

    (let ((first-token (car tokens)))
        (if (string= first-token value-type)
            (if (cdr tokens)
                (cdr tokens)
                t)
            nil)))


(defun parse-set-defvar (tokens operation)
    ;; Parse the defvar operation and return the result
    ;; OP_OP KW_DEFVAR IDENTIFIER EXP OP_CP
    ;; OP_OP KW_SET IDENTIFIER EXP OP_CP

    (let* ((first-token (car tokens))
            (second-token (car (cdr tokens))))
        (if (and (string= first-token "OP_OP") (string= second-token operation))
            (let* ((identifier (parse-values (cddr tokens) "IDENTIFIER"))
                  (exp1 (parse-exp identifier))
                  (next-tokens exp1))
                (if (and (not (null identifier)) (not (null exp1)))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_CP"))
                            (if (cdr next-tokens)
                                (cdr next-tokens)
                                t)
                            nil)
                        nil)
                    nil))
            nil)))

(defun parse-expb (tokens)
    ;; Rules:
    ;; EXPB:
    ;;     OP_OP KW_AND EXP EXP OP_CP { asprintf(&$$ "(and %s %s)" $3 $4); } |
    ;;     OP_OP KW_OR EXP EXP OP_CP { asprintf(&$$ "(or %s %s)" $3 $4); } |
    ;;     OP_OP KW_NOT EXP OP_CP { asprintf(&$$ "(not %s)" $3); } |
    ;;     OP_OP KW_LESS EXP EXP OP_CP { asprintf(&$$ "(< %s %s)" $3 $4); } |
    ;;     OP_OP KW_EQUAL EXP EXP OP_CP { asprintf(&$$ "(= %s %s)" $3 $4); } |
    ;;     KW_TRUE { asprintf(&$$ "true"); } |
    ;;     KW_FALSE { asprintf(&$$ "false"); } |
    ;;     IDENTIFIER { $$ = $1; };

    (cond
        ((parse-aritmetic tokens "KW_AND" t) (parse-aritmetic tokens "KW_AND" t))
        ((parse-aritmetic tokens "KW_OR" t) (parse-aritmetic tokens "KW_OR" t))
        ((parse-aritmetic tokens "KW_NOT" nil) (parse-aritmetic tokens "KW_NOT" nil))
        ((parse-aritmetic tokens "KW_LESS" t) (parse-aritmetic tokens "KW_LESS" t))
        ((parse-aritmetic tokens "KW_EQUAL" t) (parse-aritmetic tokens "KW_EQUAL" t))
        ((parse-values tokens "KW_TRUE") (parse-values tokens "KW_TRUE"))
        ((parse-values tokens "KW_FALSE") (parse-values tokens "KW_FALSE"))
        (t nil)))

(defun parse-func-params (tokens)
    ;; Parse the func-params and return the result
    ;; Rules:
    ;; FUNC_PARAMS:
    ;;     /* empty */ { $$ = ""; } |
    ;;     FUNC_PARAMS IDENTIFIER { 
    ;;         if (strcmp($1, "") == 0)
    ;;             asprintf(&$$, "%s", $2);
    ;;         else
    ;;             asprintf(&$$, "%s %s", $1, $2);
    ;;     };

    (if (null tokens)
        t
        (let ((first-token (car tokens)))
            (if (string= first-token "IDENTIFIER")
                (if (cdr tokens)
                    (parse-func-params (cdr tokens))
                    t)
                tokens))))

(defun parse-fcall (tokens)
    ;; Parse the fcall and return the result
    ;; OP_OP IDENTIFIER EXPLIST OP_CP { asprintf(&$$ "(%s %s)", $2, $3); } |

    (let ((first-token (car tokens)))
        (if (string= first-token "OP_OP")
            (let ((identifier (parse-values (cdr tokens) "IDENTIFIER")))
                (if (not (null identifier))
                    (let* ((paramlist (parse-explist identifier))
                          (next-tokens paramlist))
                        (if (not (null paramlist))
                            (if (listp next-tokens)
                                (if (and (string= (car next-tokens) "OP_CP"))
                                    (if (cdr next-tokens)
                                        (cdr next-tokens)
                                        t)
                                    nil)
                                nil)
                            nil))
                    nil))
            nil)))

(defun parse-value-in-list (tokens)
    ;; Parse the value in the list and return the result
    ;; Rules:
    ;; VALUES:
    ;;     VALUEI { $$ = $1; } |
    ;;     VALUEF { $$ = $1; } |
    ;;     IDENTIFIER { $$ = $1; } |
    ;;     VALUES OP_COMMA VALUEF { asprintf(&$$ "%s %s" $1 $3); } |
    ;;     VALUES OP_COMMA VALUEI { asprintf(&$$ "%s %s" $1 $3); } |
    ;;     VALUES OP_COMMA IDENTIFIER { asprintf(&$$ "%s %s" $1 $3); };

    (let ((parse-value-valuei (parse-values tokens "VALUEI"))
            (parse-value-valuef (parse-values tokens "VALUEF"))
            (parse-value-identifier (parse-values tokens "IDENTIFIER")))
        (if (or (not (null parse-value-valuei)) (not (null parse-value-valuef)) (not (null parse-value-identifier)))
            (let ((next-tokens (cdr tokens)))
                (if (not (null next-tokens))
                    (if (string= (car next-tokens) "OP_COMMA")
                        (if (cdr next-tokens)
                            (let ((parse-value-valuei (parse-values (cdr next-tokens) "VALUEI"))
                                    (parse-value-valuef (parse-values (cdr next-tokens) "VALUEF"))
                                    (parse-value-identifier (parse-values (cdr next-tokens) "IDENTIFIER")))
                                (if (or parse-value-valuei parse-value-valuef parse-value-identifier)
                                    (if (listp (cdr next-tokens))
                                        (if (null (cdr next-tokens))
                                            t
                                            (parse-value-in-list (cdr next-tokens)))
                                        nil)
                                    nil))
                            nil)
                        (if (listp next-tokens)
                            (if (null next-tokens)
                                t
                                next-tokens)
                            t))
                    t)
            )
            tokens)))

(defun parse-apostrophe (tokens)
    ;; Parse the apostrophe and return the result
    ;; Rules:
    ;; LIST:
    ;;     OP_APOSTROPHE OP_OP OP_CP { asprintf(&$$ "\'()"); } |
    ;;     OP_APOSTROPHE OP_OP VALUES OP_CP { asprintf(&$$ "\'(%s)" $3); };

    (let ((first-token (car tokens))
            (second-token (car (cdr tokens))))
        (if (and (string= first-token "OP_APOSTROPHE") (string= second-token "OP_OP"))
            (let* ((list-values (parse-value-in-list (cddr tokens)))
                  (next-tokens list-values))
                (if (not (null list-values))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_CP"))
                            (if (cdr next-tokens)
                                (cdr next-tokens)
                                t)
                            nil)
                        nil)
                    (let ((third-token (car (cdr next-tokens))))
                        (if (string= third-token "OP_CP")
                            (if (cdr (cdr next-tokens))
                                (cdr (cdr next-tokens))
                                t)
                            nil))))
            nil)))

(defun parse-kw-list (tokens)
    ;; Parse the kw-list and return the result
    ;; Rules:
    ;; LIST:
    ;;     OP_OP KW_LIST VALUES OP_CP { asprintf(&$$ "(list %s)" $3); } |

    (let ((first-token (car tokens))
            (second-token (car (cdr tokens))))
        (if (and (string= first-token "OP_OP") (string= second-token "KW_LIST"))
            (let* ((list-values (parse-value-in-list (cddr tokens)))
                  (next-tokens list-values))
                (if (not (null list-values))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_CP"))
                            (if (cdr next-tokens)
                                (cdr next-tokens)
                                t)
                            nil)
                        nil)
                    nil))
            nil)))

(defun parse-list (tokens)
    ;; Parse the list and return the result
    ;; Rules:
    ;; LIST:
    ;;     OP_OP KW_LIST VALUES OP_CP { asprintf(&$$ "(list %s)" $3); } |
    ;;     OP_APOSTROPHE OP_OP OP_CP { asprintf(&$$ "\'()"); } |
    ;;     KW_NIL { asprintf(&$$ "nil"); } |
    ;;     OP_APOSTROPHE OP_OP VALUES OP_CP { asprintf(&$$ "\'(%s)" $3); };

    (cond
        ((string= (car tokens) "KW_NIL" ) (if (cdr tokens) (cdr tokens) t))
        ((string= (car tokens) "OP_APOSTROPHE") (parse-apostrophe tokens))
        ((string= (car tokens) "OP_OP") (parse-kw-list tokens))
        (t nil)))

(defun parse-list-input (tokens)
    ;; Parse the list input and return the result
    ;; Rules:
    ;; LIST_INPUT:
    ;;     OP_OP KW_APPEND LIST_INPUT LIST_INPUT OP_CP { asprintf(&$$ "(append %s %s)" $3 $4); } |
    ;;     OP_OP KW_CONCAT LIST_INPUT LIST_INPUT OP_CP { asprintf(&$$ "(concat %s %s)" $3 $4); } |
    ;;     IDENTIFIER { $$ = $1; } |
    ;;     LIST { $$ = $1; };

    (if (parse-values tokens "IDENTIFIER")
        (parse-values tokens "IDENTIFIER")
        (let ((first-token (car tokens)))
            (if (string= first-token "OP_OP")
                (if (or (string= (car (cdr tokens)) "KW_APPEND") (string= (car (cdr tokens)) "KW_CONCAT"))
                    (let* ((list-input1 (parse-list-input (cddr tokens)))
                        (list-input2 (parse-list-input list-input1))
                        (next-tokens list-input2)
                        )
                        (if (not (null list-input1))
                            (if (not (null list-input2))
                                (if (listp next-tokens)
                                    (if (and (string= (car next-tokens) "OP_CP"))
                                        (if (cdr next-tokens)
                                            (cdr next-tokens)
                                            t)
                                        nil)
                                    nil)
                                nil)
                            nil))
                    (parse-list tokens))
                (parse-list tokens)))))


(defun parse-load (tokens)
    ;; Parse the load operation and return the result
    ;; OP_OP KW_LOAD STRING OP_CP

    (let ((first-token (car tokens)))
        (if (string= first-token "OP_OP")
            (if (string= (car (cdr tokens)) "KW_LOAD")
                (let* ((string (parse-values (cddr tokens) "STRING"))
                      (next-tokens string))
                    (if (not (null string))
                        (if (listp next-tokens)
                            (if (and (string= (car next-tokens) "OP_CP"))
                                (if (cdr next-tokens)
                                    (cdr next-tokens)
                                    t)
                                nil)
                            nil)
                        nil))
                nil)
            nil)))

(defun parse-while (tokens)
    ;; Parse the while operation and return the result
    ;;     OP_OP KW_WHILE EXPB EXP OP_CP { asprintf(&$$, "(while %s %s)", $3, $4); } |

    (let ((first-token (car tokens)))
        (if (string= first-token "OP_OP")
            (if (string= (car (cdr tokens)) "KW_WHILE")
                (let* ((expb (parse-expb (cddr tokens)))
                      (exp1 (parse-exp expb))
                      (next-tokens exp1)
                    )
                    (if (and (not (null expb)) (not (null exp1)))
                        (if (listp next-tokens)
                            (if (and (string= (car next-tokens) "OP_CP"))
                                (if (cdr next-tokens)
                                    (cdr next-tokens)
                                    t)
                                nil)
                            nil)
                        nil))
                nil)
            nil)))

(defun parse-for (tokens)
    ;; Parse the for operation and return the result
    ;;     OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXPLIST OP_CP { asprintf(&$$, "(for (%s %s %s) %s)", $4, $5, $6, $8); } |

    (let ((first-token (car tokens))
            (second-token (car (cdr tokens)))
            (third-token (car (cddr tokens))))
        (if (and (string= first-token "OP_OP")
                (string= second-token "KW_FOR")
                (string= third-token "OP_OP"))
            (let* ((identifier (parse-values (cdr (cdr (cdr tokens))) "IDENTIFIER"))
                  (exp1 (parse-exp identifier))
                  (exp2 (parse-exp exp1))
                  (next-tokens exp2))
                (if (and (not (null identifier)) (not (null exp1)) (not (null exp2)))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_CP"))
                            (let* ((explist (parse-explist (cdr next-tokens)))
                                    (left-tokens explist))
                                (if (not (null explist))
                                    (if (listp left-tokens)
                                        (if (and (string= (car left-tokens) "OP_CP"))
                                            (if (cdr left-tokens)
                                                (cdr left-tokens)
                                                t)
                                            nil)
                                        nil)
                                    nil))
                            nil)
                        nil)
                    nil))
            nil)))

(defun parse-print (tokens)
    ;; Parse the print operation and return the result
    ;;     OP_OP KW_PRINT EXP OP_CP { asprintf(&$$, "(print %s)", $3); } |

    (let ((first-token (car tokens)))
        (if (string= first-token "OP_OP")
            (if (string= (car (cdr tokens)) "KW_PRINT")
                (let* ((exp1 (parse-exp (cddr tokens)))
                      (next-tokens exp1))
                    (if (not (null exp1))
                        (if (listp next-tokens)
                            (if (and (string= (car next-tokens) "OP_CP"))
                                (if (cdr next-tokens)
                                    (cdr next-tokens)
                                    t)
                                nil)
                            nil)
                        nil))
                nil)
            nil)))

(defun parse-exit (tokens)
    ;; Parse the exit operation and return the result
    ;;     OP_OP KW_EXIT OP_CP { asprintf(&$$, "(exit)"); exit(0); } |

    (let ((first-token (car tokens)))
        (if (string= first-token "OP_OP")
            (if (string= (car (cdr tokens)) "KW_EXIT")
                (let ((next-tokens (cdr (cdr tokens))))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_CP"))
                            (if (cdr next-tokens)
                                (cdr next-tokens)
                                t)
                            nil)
                        nil))
                nil)
            nil)))

(defun parse-deffun (tokens)
    ;; Parse the deffun operation and return the result
    ;;     OP_OP KW_DEFFUN IDENTIFIER OP_OP PARAMLIST OP_CP EXPLIST OP_CP { asprintf(&$$, "(deffun %s (%s) %s)", $3, $5, $7); } |

    (let ((first-token (car tokens))
            (second-token (car (cdr tokens))))
        (if (and (string= first-token "OP_OP") (string= second-token "KW_DEFFUN"))
            (let* ((identifier (parse-values (cddr tokens) "IDENTIFIER"))
                    (next-tokens identifier))
                (if (not (null identifier))
                    (if (listp next-tokens)
                        (if (and (string= (car next-tokens) "OP_OP"))
                            (let* ((paramlist (parse-func-params (cdr next-tokens)))
                                    (left-tokens paramlist))
                                (if (not (null paramlist))
                                    (if (listp left-tokens)
                                        (if (and (string= (car left-tokens) "OP_CP"))
                                            (let* ((explist (parse-explist (cdr left-tokens)))
                                                    (left-tokens explist))
                                                (if (not (null explist))
                                                    (if (listp left-tokens)
                                                        (if (and (string= (car left-tokens) "OP_CP"))
                                                            (if (cdr left-tokens)
                                                                (cdr left-tokens)
                                                                t)
                                                            nil)
                                                        nil)
                                                    nil))
                                            nil)
                                        nil)
                                    nil))
                            nil)
                        nil)
                    nil))
            nil)))

(defun parse-comment (tokens)
    ;; Parse the comment and return the result
    ;;     COMMENT { $$ = ""; } |

    (let ((first-token (car tokens)))
        (if (string= first-token "COMMENT")
            t
            nil )))

(defun parse-exp (tokens)
    ;; Parse the exp and return the result
    ;; Rules:
    ;; EXP:

    (cond
        ((parse-aritmetic tokens "OP_PLUS" t) (parse-aritmetic tokens "OP_PLUS" t))
        ((parse-aritmetic tokens "OP_MINUS" t) (parse-aritmetic tokens "OP_MINUS" t))
        ((parse-aritmetic tokens "OP_MULT" t) (parse-aritmetic tokens "OP_MULT" t))
        ((parse-aritmetic tokens "OP_DIV" t) (parse-aritmetic tokens "OP_DIV" t))
        ((parse-if-else tokens nil) (parse-if-else tokens nil))
        ((parse-if-else tokens t) (parse-if-else tokens t))
        ((parse-for tokens) (parse-for tokens))
        ((parse-while tokens) (parse-while tokens))
        ((parse-print tokens) (parse-print tokens))
        ((parse-exit tokens) (parse-exit tokens))
        ((parse-deffun tokens) (parse-deffun tokens))
        ((parse-expb tokens) (parse-expb tokens))
        ((parse-list-input tokens) (parse-list-input tokens))
        ((parse-set-defvar tokens "KW_SET") (parse-set-defvar tokens "KW_SET"))
        ((parse-set-defvar tokens "KW_DEFVAR") (parse-set-defvar tokens "KW_DEFVAR"))
        ((parse-fcall tokens) (parse-fcall tokens))
        ((parse-load tokens) (parse-load tokens))
        ((parse-comment tokens) (parse-comment tokens))
        ((parse-values tokens "VALUEI") (parse-values tokens "VALUEI"))
        ((parse-values tokens "VALUEF") (parse-values tokens "VALUEF"))
        (t nil)))

(defun parse-explist (tokens)
    ;; Parse the explist and return the result
    ;; Rules:
    ;; EXPLIST:
    ;;     EXP
    ;;     EXPLIST EXP;

    (let ((parsed-exp (parse-exp tokens)))
        (if parsed-exp
            (if (listp parsed-exp)
                (parse-explist parsed-exp)
                parsed-exp
            )
            tokens)))

(defun parse-input (tokens)
    ;; Parse the input and return the result
    (let ((result (parse-explist tokens)))
        (if (or (null result) (listp result))
            (format t "Syntax error: Invalid input:~%")
            (format t "Syntax correct~%"))))

(defun top-down-parser (tokens)
    (if (null tokens)
        (format t "Syntax correct~%")
        (parse-input tokens)))

(defun is-letter (char)
    ;; Check if the character is a letter (a-zA-Z)
    (or (and (>= (char-code char) 65) (<= (char-code char) 90))
        (and (>= (char-code char) 97) (<= (char-code char) 122))))

(defun is-white-space (char)
    ;; Check if the character is a whitespace (space tab newline)
    (or (char= char #\Space)
        (char= char #\Tab)
        (char= char #\Newline)))

(defun is-operator (char)
    ;; Check if the character is an operator (+ - / * ( ) ) , " 
    (or (= (char-code char) 43)  ; +
        (= (char-code char) 45)  ; -
        (= (char-code char) 47)  ; /
        (= (char-code char) 42)  ; *
        (= (char-code char) 40)  ; (
        (= (char-code char) 41)  ; )
        (= (char-code char) 44) ; ,
        (= (char-code char) 39)) ; '
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

(defun state-6 (line index token)
  ;; Go till the end of the string with the following rules:
  (let ((line-length (length line)))
    (labels ((state-6-helper (line index token)
                (if (< index line-length)
                   (let ((char (char line index)))
                        (if (char= char #\")
                            (if (< (+ index 1) line-length)
                                (let* ((next-char (char line (+ index 1))))
                                    (if (or (= (char-code next-char) 40) (= (char-code next-char) 41) (= (char-code next-char) 44) (is-white-space next-char)) ; ( ) ,
                                        (values (+ index 1) "STRING")
                                        (error "Syntax error: ~a cannot be followed by ~a" (concatenate 'string token (string char)) next-char)
                                    )
                                )
                                (values (+ index 1) "STRING")
                            )
                            (state-6-helper line (+ index 1) (concatenate 'string token (string char)))
                        )
                    )
                   (error "Syntax error: Missing '\"' at the end of the string"))))

      ;; Start processing with state-6-helper
      (state-6-helper line index token))))


(defun state-0 (line index)
    ;; Initial state: (q0)
    (let ((char (char line index)))
        (cond
            ((is-letter char) (state-1 line (+ index 1) (string char)))
            ((is-digit char) (state-2 line (+ index 1) (string char)))
            ((is-operator char) (state-5 line (+ index 1) char))
            ((char= char #\:) (state-4 line (+ index 1) (string char)))
            ((char= char #\;) (state-3 line (+ index 1)))
            ((char= char #\") (state-6 line (+ index 1) (string char)))
            ((is-white-space char) (values (+ index 1) nil nil))
            (t (error "Syntax error: Invalid character: ~a" char)))))

(defun state-1 (line index token)
    ;; Go till the end of the identifier or keyword with the following rules:
    (let ((line-length (length line)))
        (labels ((state-1-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (or (is-letter char) (is-digit char) (= (char-code char) 95)) ; letter digit or '_'
                        (state-1-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (is-white-space char) ; whitespace
                            (values (+ index 1) (identifier-or-keyword token))
                            (if (or (= (char-code char) 40) (= (char-code char) 41) (= (char-code char) 44)) ; ( ) , "
                                (values index (identifier-or-keyword token))
                                (error "Syntax error: ~a cannot be followed by ~a" token char)
                            )
                        )
                    )
                )
                (values index (identifier-or-keyword token)) ; end of the line
            )
        ))
        (state-1-helper line index token)))) ; start the helper function

(defun state-2 (line index token)
    ;; Go till the end of the integer with the following rules:
    (let ((line-length (length line)))
        (labels ((state-2-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (is-digit char) ; digit
                        (state-2-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (or (char= char #\:) (char= char #\f)) ; : or f
                            (state-4 line (+ index 1) (concatenate 'string token (string char)))
                            (if (is-white-space char) ; whitespace
                                (values (+ index 1) "VALUEI")
                                (if (or (= (char-code char) 40) (= (char-code char) 41) (= (char-code char) 44)) ; ( ) ,
                                    (values index "VALUEI")
                                    (error "Syntax error: ~a cannot token be followed by ~a" token char))))))
                (values index "VALUEI") ; end of the line
            )))
        (state-2-helper line index token)))
)

(defun state-3 (line index)
    ;; if the next char is ';' stay in state-3 and ignore the rest of the line
    ;; if the next char is not ';' Syntax error
    (let ((line-length (length line)))
        (if (< index line-length)
            (let ((char (char line index)))
                (if (char= char #\;)
                    (values line-length "COMMENT") ; end of the line
                    (error "Syntax error: Invalid character: ~a" char))) ; syntax error
            (error "Syntax error: Missing ';' at the end of the line")))) ; syntax error

(defun state-4 (line index token)
    ;; Go till the end of the fraction with the following rules:
    (let ((line-length (length line)))
        (labels ((state-4-helper (line index token)
            (if (< index line-length)
                (let ((char (char line index)))
                    (if (is-digit char)
                        (state-4-helper line (+ index 1) (concatenate 'string token (string char)))
                        (if (is-white-space char)
                            (values (+ index 1) "VALUEF")
                            (if (or (= (char-code char) 40) (= (char-code char) 41) (= (char-code char) 44)) ; ( ) ,
                                (values index "VALUEF")
                                (error "Syntax error: ~a cannot be followed by ~a" token char)))))
                (values index "VALUEF"))))
        (state-4-helper line index token))))

(defun state-5 (line index token)
    ;; State for operators
    (if (or (= (char-code token) 40) (= (char-code token) 41) (= (char-code token) 44) (= (char-code token) 34)) ; ( ) , "
        (values index (operator-value (string token)))
        (if (< index (length line))
            (let ((next-char (char line index)))
                (if (or (= (char-code next-char) 40) (= (char-code next-char) 41) (is-white-space next-char) (= (char-code next-char) 44) (= (char-code next-char) 34))
                    (values index (operator-value (string token)))
                    (error "Syntax error: ~a cannot be followed by ~a" token next-char)))
            (values index (operator-value (string token))))))

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

(defun repl-process ()
    ;; read eval print loop for lexer
    (format t "> ")
    (let ((line (read-line)))
        (if line
            (if (string= (string-trim " " line) "quit") ; exit the interpreter
                (format t "Exiting the interpreter~%")
                (progn
                    (top-down-parser (dfa line))
                    (repl-process))))))

(defun load-file (file-name)
    ;; read the file and print the tokens
    (if file-name
        (with-open-file (stream file-name :direction :input :if-does-not-exist nil)
            (if (not stream)
                (error "File not found: ~a" file-name))
                (labels ((read-file-helper (accumulated-tokens)
                        (let ((line (read-line stream nil)))
                            (if line
                                (let ((line-tokens (top-down-parser (dfa line))))
                                    (read-file-helper (append accumulated-tokens line-tokens))) ;; read the next line
                                (print-list accumulated-tokens)
                            )
                        )))
                (read-file-helper nil)))))

(defun gppinterpreter ()
    ;; main function for the interpreter
    (let ((args *args*)) ;; get the arguments
        (if (is-valid-argument args)
            (progn
                (format t "Print \"quit\" to exit the interpreter~%")
                (load-file (nth 0 args)) ;; load the file
                (repl-process)
            )
            (format t "Invalid number of arguments~%"))))


(gppinterpreter)
