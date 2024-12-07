(defparameter *keywords* '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "print" "true" "false"))
(defparameter *operators* '("+" "-" "/" "*" "(" ")" ","))
(defvar *tokens_as_symbols* nil)

(defun split-sequence (delimiter sequence &key (test #'eql))
  "Split a sequence (e.g., string or list) into subsequences based on the specified delimiter."
  (let ((result '())
        (current '()))
    (dolist (element sequence (nreverse (if current (cons (nreverse current) result) result)))
      (if (funcall test element delimiter)
          (when current
            (push (nreverse current) result)
            (setf current nil))
          (push element current)))))

(defun tokenize_input (input)
  (let* ((token-list '())
         (current-token "")
         (input-length (length input)))
    (loop for i from 0 below input-length
          for char = (char input i)
          for next-char = (if (< (1+ i) input-length)
                              (char input (1+ i))
                              nil)
          do
          (cond
            ;; Handle comments starting with ";;"
            ((and (char= char #\;) (char= next-char #\;))
             (when (not (string= current-token ""))
               (push current-token token-list)
               (setf current-token ""))
             (push "COMMENT" token-list)
             (return-from tokenize_input (nreverse token-list))) ;; Ignore rest of line

            ;; Handle alphanumeric tokens (identifiers)
            ((or (alpha-char-p char) (digit-char-p char) (char= char #\_))
             (setf current-token (concatenate 'string current-token (string char))))

            ;; Handle operators and parentheses
            ((or (find char *operators*) (char= char #\() (char= char #\)))
             (when (not (string= current-token ""))
               (push current-token token-list)
               (setf current-token ""))
             (push (string char) token-list))

            ;; Handle commas as parameter separators
            ((char= char #\,)
             (when (not (string= current-token ""))
               (push current-token token-list)
               (setf current-token ""))
             (push "," token-list))

            ;; Handle whitespace
            ((char= char #\SPACE)
             (when (not (string= current-token ""))
               (push current-token token-list)
               (setf current-token "")))

            ;; Handle any other characters
            (t
             (setf current-token (concatenate 'string current-token (string char))))))
    (when (not (string= current-token ""))
      (push current-token token-list))
    (nreverse token-list)))



(defun is_keyword (token)
  (member token *keywords* :test 'string=))

(defun is_operator (token)
  (find (char token 0) *operators*))

(defun is_unsigned_fraction (token)
  (and (search ":" token)
       (let ((parts (split-sequence ":" token)))
         (and (= (length parts) 2)
              (every #'(lambda (part) (every #'digit-char-p part)) parts)))))

(defun is_unsigned_integer (token)
  (every #'digit-char-p token))

(defun is_identifier (token)
  (and (alpha-char-p (char token 0))
       (every #'(lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_)))
              token)))

(defun dfa (token)
  (let ((*state* "START"))
    (cond
      ;; Check for keywords
      ((is_keyword token)
       (push (format nil "KW_~A" (string-upcase token)) *tokens_as_symbols*))

      ;; Special case for the "+" operator to classify as OP_PLUS
      ((string= token "+")
       (push "OP_PLUS" *tokens_as_symbols*))
      ((string= token "-")
       (push "OP_MINUS" *tokens_as_symbols*))
      ((string= token "(")
       (push "OP_OP" *tokens_as_symbols*))
      ((string= token ")")
       (push "OP_CP" *tokens_as_symbols*))

      ;; Check for other operators
      ((is_operator token)
       (push (format nil "OP_~A" token) *tokens_as_symbols*))

      ;; Check for tokens containing 'f'
      ((search "f" token)
       (push "VALUEF" *tokens_as_symbols*))

      ;; Check for unsigned fractions
      ((is_unsigned_fraction token)
       (push "VALUEF" *tokens_as_symbols*))

      ;; Check for unsigned integers
      ((is_unsigned_integer token)
       (push "VALUEI" *tokens_as_symbols*))

      ;; Check for identifiers
      ((is_identifier token)
      (format t "token: ~A~%" token)
       (push "IDENTIFIER" *tokens_as_symbols*))

      ;; Handle unrecognized tokens
      (t (format t "SYNTAX_ERROR: ~A cannot be tokenized~%" token)))))



(defun gppinterpreter ()
  (let ((args *args*))
    (let ((file (if (first args) (first args) nil)))
      (if file
          (with-open-file (stream file)
            (loop for line = (read-line stream nil)
                  while line do
                  (let ((tokens (tokenize_input line)))
                    (loop for token in tokens do (dfa token)))))
          (loop
            (format t "> ")
            (let ((input (read-line)))
              (when (string= input "exit")
                (return))
              (let ((tokens (tokenize_input input)))
                (loop for token in tokens do (dfa token)))))))
    ;; Print tokens as symbols, each on a new line
    (format t "Tokens as symbols:~%")
    (dolist (token (reverse *tokens_as_symbols*))
      (format t "~A~%" token))))

(gppinterpreter)