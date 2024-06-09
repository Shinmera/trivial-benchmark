(in-package #:org.shirakumo.trivial-benchmark)

(defun print-table (table &key (stream T) (padding 2))
  (let ((widths (apply #'map 'list #'max (loop for row in table
                                               collect (loop for field in row
                                                             collect (+ padding (length (princ-to-string field))))))))
    (dolist (row table)
      (apply #'format stream (format NIL "~~&~{~~~da~}~~%" widths) row))))

(defun round-to (num n)
  (let ((n (expt 10 n)))
    (/ (round (* num n)) n)))

(defun enlist (thing &rest args)
  (if (listp thing)
      thing
      (list* thing args)))

(defun unlist (thing)
  (if (listp thing)
      (first thing)
      thing))
