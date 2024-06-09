(in-package #:org.shirakumo.trivial-benchmark)

(defun print-table (table &key (stream T) (padding 1) (format :fancy))
  (let* ((columns (length (first table)))
         (widths (append (loop for i from 0 below columns
                               collect (loop for row in table
                                             maximize (+ (* 2 padding) (length (princ-to-string (nth i row))))))
                         '(0)))
         (values (loop for row in table
                       collect (loop for value in row
                                     for width in widths
                                     collect width collect value))))
    (ecase format
      (:princ
       (princ table stream))
      (:minimal
       (loop for row = (pop values)
             do (loop for (width val) on row by #'cddr
                      do (format stream "~v{ ~}" padding 0)
                         (format stream "~vf" (- width padding padding) val)
                         (format stream "~v{ ~}" padding 0))
                (format stream "~%")
             while values))
      (:fancy
       (format stream "┌~{~v{─~}~^┬~:*~}┐~%" widths)
       (loop for row = (pop values)
             do (format stream "│")
                (loop for (width val) on row by #'cddr
                      do (format stream "~v{ ~}" padding 0)
                         (format stream "~vf" (- width padding padding) val)
                         (format stream "~v{ ~}│" padding 0))
                (format stream "~%")
                (when values
                  (format stream "├~{~v{─~}~^┼~:*~}┤~%" widths))
             while values)
       (format stream "└~{~v{─~}~^┴~:*~}┘" widths)))))

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
