;; Helper function to remove C data types from text
(defun remove-c-types (line)
  "Removes common C data types from the line by replacing them with spaces."
  (dolist (type '("int" "float" "double" "char" "void" "unsigned" "long"))
    (setf line (replace-all line type "")))
  line)

(defun replace-all (string old new)
  "Replaces all occurrences of 'old' substring in 'string' with 'new' substring."
  (let ((output ""))
    (loop
      for start = 0 then (+ pos (length old))
      for pos = (search old string :start2 start)
      do (setf output (concatenate 'string output (subseq string start pos) new))
      while pos
      finally (setf output (concatenate 'string output (subseq string start))))
    output))


;; Helper function to extract text between two characters
(defun extract-between (line start-char end-char)
  "Extracts a substring between start-char and end-char in line."
  (let ((start (position start-char line))
        (end (position end-char line :start (1+ (position start-char line)))))
    (if (and start end)
        (subseq line (1+ start) end)
        "")))

;; Function to determine the type of a C line
(defun line-type (line)
  (cond
    ((search "int main" line) 'function-def)
    ((search "if" line) 'if-statement)
    ((search "for" line) 'for-loop)
    ((search "while" line) 'while-loop)
    ((search "=" line) 'assignment)
    ((search "(" line) 'function-call)
    (t 'unknown)))

;; Function to select the conversion function based on line type
(defun conversion-foo (line-type)
  (case line-type
    ('function-def #'convert-function-def)
    ('if-statement #'convert-if)
    ('for-loop #'convert-for)
    ('while-loop #'convert-while)
    ('assignment #'convert-assignment)
    ('function-call #'convert-function-call)
    (t (lambda (line) (error "Unknown line type encountered: ~A" line)))))

;; Conversion function for C function definitions
(defun convert-function-def (line)
  (let* ((paren-pos (position #\( line))
         (func-name (string-trim '(#\space) (subseq line 0 paren-pos)))
         (args-start (1+ paren-pos))
         (args-end (position #\) line :start args-start))
         (args (extract-between line #\( #\))))
    (format nil "(defun ~A (~A)"
            func-name
            (remove-c-types args))))

;; Conversion function for "if" statements
(defun convert-if (line)
  (let ((condition (extract-between line #\( #\)))
        (body (extract-between line #\{ #\})))
    (format nil "(if ~A ~A)" condition (convert body))))

;; Conversion function for "for" loops
(defun convert-for (line)
  (let* ((content (extract-between line #\( #\)))
         (components (split-on-semicolons content))
         (init (first components))
         (cond (second components))
         (inc (third components))
         (body (extract-between line #\{ #\})))
    (format nil "(loop ~A while ~A do (progn ~A ~A))"
            (convert init)
            cond
            (convert body)
            inc)))

;; Function to split by semicolon for for-loop parsing
(defun split-on-semicolons (text)
  (loop for pos = (position #\; text)
        then (position #\; text :start (1+ pos))
        for start = 0 then (1+ pos)
        collect (string-trim '(#\space) (subseq text start pos)) into parts
        until (null pos)
        finally (return (append parts (list (subseq text start))))))

;; Conversion function for "while" loops
(defun convert-while (line)
  (let ((condition (extract-between line #\( #\)))
        (body (extract-between line #\{ #\})))
    (format nil "(loop while ~A do ~A)" condition (convert body))))

;; Conversion function for variable assignment
(defun convert-assignment (line)
  (let* ((without-type (remove-c-types line))
         (assignment-parts (split-on-equals without-type))
         (var (string-trim '(#\space) (first assignment-parts)))
         (value (string-trim '(#\space #\;) (second assignment-parts))))
    (format nil "(setq ~A ~A)" var value)))

;; Function to split by equals sign for assignment parsing
(defun split-on-equals (text)
  (let ((equal-pos (position #\= text)))
    (list (subseq text 0 equal-pos)
          (subseq text (1+ equal-pos)))))

;; Conversion function for function calls
(defun convert-function-call (line)
  (let* ((paren-pos (position #\( line))
         (func-name (string-trim '(#\space) (subseq line 0 paren-pos)))
         (args (extract-between line #\( #\))))
    (format nil "(~A ~A)" func-name args)))

;; Recursive main conversion function
(defun convert (line)
  (let ((type (line-type line))
        (conversion-fn (conversion-foo (line-type line))))
    (funcall conversion-fn line)))

;; Reads the file and returns lines as a list
(defun read-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line
          collect line)))

;; Writes the final converted code to the output file
(defun write-file (filename content)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (dolist (line content)
      (write-line line out))))

;; Recursive processing of the file content line by line
(defun recursive-convert (lines)
  (if (null lines)
      '()
      (cons (convert (car lines))
            (recursive-convert (cdr lines)))))

;; Main function: Read, convert, and write to a file
(defun convert-c-to-lisp (input-file output-file)
  (let ((lines (read-file input-file)))
    (let ((converted (recursive-convert lines)))
      (write-file output-file converted))))


(convert-c-to-lisp "main.c" "output.lisp")

