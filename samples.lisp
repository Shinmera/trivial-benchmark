#|
 This file is a part of Trivial-Benchmark
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trivial-benchmark)

(defclass listed-metric (metric)
  ((samples :initarg :samples :initform () :accessor samples))
  (:documentation "A METRIC that implements its sample storage as a list. 
SAMPLES is SETF-able for this class.

See METRIC"))

(defclass delta-metric (listed-metric)
  ((starting-value :initform NIL :accessor starting-value))
  (:documentation "A LISTED-METRIC that calculates a sample point according to the delta between the START and COMMIT.
Sub-classes of this must implement the TAKE-SAMPLE method.

See LISTED-METRIC"))

(defgeneric starting-value (delta-metric)
  (:documentation "Returns the value stored when START was called on the DELTA-METRIC."))

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

(defmacro define-delta-metric (name &body sample-point-forms)
  "Shortcut to define a DELTA-METRIC.
The SAMPLE-POINT-FORMS should return a number to use to calculate a delta.

See DELTA-METRIC"
  (let ((doc (when (stringp (first sample-point-forms))
               (first sample-point-forms))))
    `(progn
       (pushnew ',name *default-metrics*)
       
       (defclass ,name (delta-metric)
         ()
         ,@(when doc `((:documentation ,doc))))

       (defmethod take-sample ((metric ,name))
         ,@sample-point-forms))))

(define-delta-metric real-time
  "Samples the results of GET-INTERNAL-REAL-TIME in seconds."
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(define-delta-metric run-time
  "Samples the results of GET-INTERNAL-RUN-TIME in seconds."
  (/ (get-internal-run-time)
     internal-time-units-per-second))

#+sbcl
(progn
  (define-delta-metric user-run-time
    "Samples the first value (user-run-time) of SB-SYS:GET-SYSTEM-INFO in seconds."
    (/ (nth-value 0 (sb-sys:get-system-info))
       1000000))

  (define-delta-metric system-run-time
    "Samples the second value (system-run-time) of SB-SYS:GET-SYSTEM-INFO in seconds."
    (/ (nth-value 1 (sb-sys:get-system-info))
       1000000))

  (define-delta-metric page-faults
    "Samples the third value (page-faults) of SB-SYS:GET-SYSTEM-INFO in seconds."
    (nth-value 2 (sb-sys:get-system-info)))

  (define-delta-metric gc-run-time
    "Samples SB-IMPL::*GC-RUN-TIME* in seconds."
    (/ sb-impl::*gc-run-time*
       1000))
  
  (define-delta-metric bytes-consed
    "Samples SB-IMPL::GET-BYTES-CONSED."
    (sb-impl::get-bytes-consed))

  (define-delta-metric eval-calls
    "Samples SB-IMPL::*EVAL-CALLS*."
    sb-impl::*eval-calls*)

  (defclass cpu-cycles (listed-metric)
    ((h0 :accessor cpu-cycles-h0)
     (l0 :accessor cpu-cycles-l0))
    (:documentation "Samples SB-IMPL::ELAPSED-CYCLES."))

  (defmethod start ((metric cpu-cycles))
    (multiple-value-bind (h0 l0) (sb-impl::read-cycle-counter)
      (setf (cpu-cycles-h0 metric) h0
            (cpu-cycles-l0 metric) l0)))

  (defmethod commit ((metric cpu-cycles))
    (multiple-value-bind (h1 l1) (sb-impl::read-cycle-counter)
      (let ((cycles (sb-impl::elapsed-cycles (cpu-cycles-h0 metric) (cpu-cycles-l0 metric) h1 l1)))
        (when (<= 0 cycles)
          (push cycles (samples metric)))))))
