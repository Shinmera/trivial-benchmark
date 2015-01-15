#|
 This file is a part of Trivial-Benchmark
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trivial-benchmark
  (:use #:cl)
  (:nicknames #:org.tymoonnext.trivial-benchmark #:benchmark)
  (:export
   #:time-spent
   #:compute-stats
   #:with-timing))
(in-package #:org.tymoonnext.trivial-benchmark)

(defmacro time-spent (&body body)
  "Returns three values: the return of the last body form, the real time passed and the run time passed."
  (let ((real1 (gensym)) (real2 (gensym)) (run1 (gensym)) (run2 (gensym)) (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
            (,run1 (get-internal-run-time))
            (,result (progn ,@body))
            (,real2 (get-internal-real-time))
            (,run2 (get-internal-run-time)))
       (declare (fixnum ,real1 ,run1 ,real2 ,run2))
       (values ,result 
               (/ (- ,real2 ,real1) internal-time-units-per-second)
               (/ (- ,run2 ,run1) internal-time-units-per-second)))))

(defun compute-stats (nums)
  "Computes statistical values from the list of numbers:
TOTAL MAX MIN MEDIAN AVERAGE STANDARD-DEVIATION"
  (let* ((count (length nums))
         (total (reduce #'+ nums))
         (average (/ total count))
         (variance (/ (reduce #'+ (mapcar #'(lambda (a) (expt (- a average) 2)) nums)) count))
         (deviation (sqrt variance))
         (median (elt (sort (copy-seq nums) #'<) (1- (ceiling (/ count 2)))))
         (max (reduce #'max nums))
         (min (reduce #'min nums)))
    (list total max min median average deviation)))

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

(defun format-stats (stream &rest data)
  (let ((list (cons '("-" "TOTAL" "MAXIMUM" "MINIMUM" "MEDIAN" "AVERAGE" "DEVIATION")
                    (loop for (key val) on data by #'cddr
                          collect (list* key (mapcar #'(lambda (a) (format NIL "~f" (round-to a 6)))
                                                     (compute-stats val)))))))
    (print-table list :stream stream)))

#+sbcl
(defun %time-function (function)
  (let ((var))
    (flet ((tmp (&key real-time-ms user-run-time-us system-run-time-us gc-run-time-ms
                      processor-cycles bytes-consed &allow-other-keys)
             (setf var (list (float (/ real-time-ms 1000))
                             (float (/ user-run-time-us 1000000))
                             (float (/ system-run-time-us 1000000))
                             (float (/ gc-run-time-ms 1000))
                             processor-cycles
                             bytes-consed))))
      (sb-impl::call-with-timing #'tmp function))
    var))

#+sbcl
(defun call-with-timing (iterations function)
  (loop for i from 1 upto iterations
        for (rt urt srt grt pc bc) = (%time-function function)
        collect rt into real-time
        collect urt into user-run-time
        collect srt into system-run-time
        collect grt into gc-run-time
        collect pc into processor-cycles
        collect bc into bytes-consed
        finally (format-stats T
                              :real-time real-time
                              :user-run-time user-run-time
                              :system-run-time system-run-time
                              :gc-run-time gc-run-time
                              :processor-cycles processor-cycles
                              :bytes-consed bytes-consed)))

#-sbcl
(defun call-with-timing (iterations function)
  (loop for i from 1 upto iterations
        for (result real run) = (multiple-value-list (time-spent (funcall function)))
        collect real into reals
        collect run into runs
        finally (format-stats T :REAL-TIME reals :RUN-TIME runs)))

(defmacro with-timing (iterations &body body)
  "Executes the body ITERATIONS times, collecting statistical data in the process.
After completion prints a table of the collection information and returns NIL."
  `(call-with-timing ,iterations #'(lambda () ,@body)))
