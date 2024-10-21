(defun split-string (separator line index result token is-splittable)
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
                            (concatenate 'string token (subseq line index (+ index 1))) 
                            t))))))

;; (defun split-string (separator line index result token is-splittable)
;;   (if (>= index (length line))
;;       (if (string= token "")
;;           result
;;           (append result (list token))
;;       )
;;       (let ((current-char (subseq line index (+ index 1))))
;;         (if (not is-splittable)
;;             (if (string= current-char separator)
;;                 (split-string separator line (+ index 1) result "" nil)
;;                 (split-string separator line (+ index 1) result (concatenate 'string token current-char) t))
;;             (if (string= current-char separator)
;;                 (split-string separator line (+ index 1) (append result (list token)) "" nil)
;;                 (split-string separator line (+ index 1) result (concatenate 'string token current-char) t))))))



(defun extract-end-value (expr varname)
    ; a<10 veya a>=10 veya a<=10 veya a>10 veya 10>a veya 10>=a veya 10<=a veya 10<a buradan hep 10 almamız lazım
    (cond
        ((search ">=" expr) (progn
                                (let* ((split-parts (split-string ">=" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
        ((search "<=" expr) (progn
                                (let* ((split-parts (split-string "<=" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
        ((search ">" expr) (progn
                                (let* ((split-parts (split-string ">" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
        ((search "<" expr) (progn
                                (let* ((split-parts (split-string "<" expr 0 nil "" nil)))
                                    (if (string= (nth 0 split-parts) varname)
                                        (nth 1 split-parts)
                                        (nth 0 split-parts)
                                    )
                                )
                            ))
    )     
)

(format t "split-string: ~a~%" (split-string ">=" "a>=10" 0 nil "" nil))

(format t "Extract end value: ~a~%" (extract-end-value "a<=10" "a"))
(format t "Extract end value: ~a~%" (extract-end-value "a<10" "a"))