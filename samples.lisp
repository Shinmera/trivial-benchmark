#|
 This file is a part of Trivial-Benchmark
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trivial-benchmark)

(defclass listed-metric (metric)
  ((samples :initarg :samples :initform () :accessor samples)))

(defclass delta-metric (listed-metric)
  ((starting-value :initform NIL :accessor starting-value)))

(defmethod start ((metric delta-metric))
  (setf (starting-value metric)
        (take-sample metric)))

(defmethod discard ((metric delta-metric))
  (setf (starting-value metric)
        NIL))

(defmethod commit ((metric delta-metric))
  (let ((point (- (take-sample metric)
                  (starting-value metric))))
    (when (<= 0 point)
      (push point (samples metric))))
  (discard metric))

(defmacro define-delta-metric (name &body metric-point-forms)
  `(progn
     (pushnew ',name *default-metrics*)
     
     (defclass ,name (delta-metric)
       ())

     (defmethod take-sample ((metric ,name))
       ,@metric-point-forms)))

(define-delta-metric real-time
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(define-delta-metric run-time
  (/ (get-internal-run-time)
     internal-time-units-per-second))

#+sbcl
(progn
  (define-delta-metric user-run-time
    (/ (nth-value 0 (sb-sys:get-system-info))
       1000000))

  (define-delta-metric system-run-time
    (/ (nth-value 1 (sb-sys:get-system-info))
       1000000))

  (define-delta-metric page-faults
    (nth-value 2 (sb-sys:get-system-info)))

  (define-delta-metric gc-run-time
    (/ sb-impl::*gc-run-time*
       1000))
  
  (define-delta-metric bytes-consed
    (sb-impl::get-bytes-consed))

  (define-delta-metric eval-calls
    sb-impl::*eval-calls*)

  (defclass cpu-cycles (listed-metric)
    ((h0 :accessor cpu-cycles-h0)
     (l0 :accessor cpu-cycles-l0)))

  (defmethod start ((metric cpu-cycles))
    (multiple-value-bind (h0 l0) (sb-impl::read-cycle-counter)
      (setf (cpu-cycles-h0 metric) h0
            (cpu-cycles-l0 metric) l0)))

  (defmethod commit ((metric cpu-cycles))
    (multiple-value-bind (h1 l1) (sb-impl::read-cycle-counter)
      (let ((cycles (sb-impl::elapsed-cycles (cpu-cycles-h0 metric) (cpu-cycles-l0 metric) h1 l1)))
        (when (<= 0 cycles)
          (push cycles (samples metric)))))))
