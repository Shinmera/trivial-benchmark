(in-package #:org.shirakumo.trivial-benchmark)

(defun print-table (table &key (stream T) (padding 2))
  "Prints a table (each list in TABLE being a row) with proper spacing."
  (let ((widths (apply #'map 'list #'max (loop for row in table
                                               collect (loop for field in row
                                                             collect (+ padding (length (princ-to-string field))))))))
    (dolist (row table)
      (apply #'format stream (format NIL "~~&~{~~~da~}~~%" widths) row))))

(defun round-to (num n)
  "Rounds NUM to N digits after the dot."
  (let ((n (expt 10 n)))
    (/ (round (* num n)) n)))

(defun type= (a b)
  "Returns T if A and B denote the same type (are subtypes of one another)
If one of the arguments does not denote a type, the result of TYPE-OF is used in their place."
  (unless (or (listp a) (symbolp a)) (setf a (type-of a)))
  (unless (or (listp b) (symbolp b)) (setf b (type-of b)))
  (and (subtypep a b)
       (subtypep b a)))
