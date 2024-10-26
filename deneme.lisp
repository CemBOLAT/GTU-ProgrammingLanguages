(defun add_till_space (line original_line index)
    ;; add till first character that is not a space
    (if (>= index (length original_line))
        ""
        (let* ((current-char (subseq original_line index (+ index 1))))
            (format t "code:~a" current-char)
            (if (string= current-char " ")
                (concatenate 'string current-char (add_till_space line original_line (+ index 1)))
                line
            )
        )
    )
)

(format t "add-till-space:~a:~%" (add_till_space "asd" "    asd" 0))