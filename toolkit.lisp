#|
 This file is a part of Trivial-Benchmark
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trivial-benchmark)

(defun print-table (table &key (stream T) (padding 2))
  "Prints a table with proper spacing."
  (let ((widths (apply #'map 'list #'max (loop for row in table
                                               collect (loop for field in row
                                                             collect (+ padding (length (princ-to-string field))))))))
    (dolist (row table)
      (apply #'format stream (format NIL "~~&~{~~~da~}~~%" widths) row))))

(defun round-to (num n)
  (let ((n (expt 10 n)))
    (/ (round (* num n)) n)))
