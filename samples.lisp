#|
 This file is a part of Trivial-Benchmark
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trivial-benchmark)

(defclass vector-metric (metric)
  ((samples :initarg :samples :initform (make-array 1000 :adjustable T :fill-pointer 0) :accessor samples))
  (:documentation "A METRIC that implements its sample storage as a vector. 
SAMPLES is SETF-able for this class.

See METRIC"))

(defclass delta-metric (vector-metric)
  ((starting-value :initform NIL :accessor starting-value)
   (stopping-value :initform NIL :accessor stopping-value)
   (units :initform 1 :accessor units))
  (:documentation "A LISTED-METRIC that calculates a sample point according to the delta between the START and COMMIT.
Sub-classes of this must implement the TAKE-SAMPLE method.

See LISTED-METRIC"))

(defgeneric starting-value (delta-metric)
  (:documentation "Returns the value stored when START was called on the DELTA-METRIC."))

(defmethod start ((metric delta-metric))
  (setf (starting-value metric)
        (take-sample metric)))

(defmethod stop ((metric delta-metric))
  (setf (stopping-value metric)
        (take-sample metric)))

(defmethod discard ((metric delta-metric))
  (setf (starting-value metric) NIL
        (stopping-value metric) NIL))

(defmethod commit ((metric delta-metric))
  (let ((point (/ (- (stopping-value metric)
                     (starting-value metric))
                  (units metric))))
    (when (<= 0 point)
      (vector-push-extend point (samples metric)))))

(defmacro define-delta-metric (name &body sample-point-forms)
  "Shortcut to define a DELTA-METRIC.
The SAMPLE-POINT-FORMS should return a number to use to calculate a delta.
NAME can be either just a NAME or a list of the form (NAME UNITS) where
UNITS is the number the sample points are divided by when saving them.
For example, if you sample a function of milliseconds, but want to report
in seconds, set UNITS to 1000. Doing this instead of dividing directly in
the form avoids potential consing when running into bignums.

See DELTA-METRIC"
  (destructuring-bind (name &optional (units 1)) (if (listp name) name (list name))
    (let ((doc (when (stringp (first sample-point-forms))
                 (first sample-point-forms))))
      `(progn
         (pushnew ',name *default-metrics*)
         
         (defclass ,name (delta-metric)
           ((units :initform ,units :accessor units))
           ,@(when doc `((:documentation ,doc))))

         (defmethod take-sample ((metric ,name))
           ,@sample-point-forms)))))

(define-delta-metric (real-time internal-time-units-per-second)
  "Samples the results of GET-INTERNAL-REAL-TIME in seconds."
  (get-internal-real-time))

(define-delta-metric (run-time internal-time-units-per-second)
  "Samples the results of GET-INTERNAL-RUN-TIME in seconds."
  (get-internal-run-time))

#+sbcl
(progn
  (define-delta-metric (user-run-time 1000000)
    "Samples the first value (user-run-time) of SB-SYS:GET-SYSTEM-INFO in seconds."
    (nth-value 0 (sb-sys:get-system-info)))

  (define-delta-metric (system-run-time 1000000)
    "Samples the second value (system-run-time) of SB-SYS:GET-SYSTEM-INFO in seconds."
    (nth-value 1 (sb-sys:get-system-info)))

  (define-delta-metric page-faults
    "Samples the third value (page-faults) of SB-SYS:GET-SYSTEM-INFO in seconds."
    (nth-value 2 (sb-sys:get-system-info)))

  (define-delta-metric (gc-run-time 1000)
    "Samples SB-IMPL::*GC-RUN-TIME* in seconds."
    sb-impl::*gc-run-time*)
  
  (define-delta-metric bytes-consed
    "Samples SB-IMPL::GET-BYTES-CONSED."
    (sb-impl::get-bytes-consed))

  (define-delta-metric eval-calls
    "Samples SB-IMPL::*EVAL-CALLS*."
    sb-impl::*eval-calls*)

  (defclass cpu-cycles (vector-metric)
    ((h0 :accessor cpu-cycles-h0)
     (l0 :accessor cpu-cycles-l0)
     (h1 :accessor cpu-cycles-h1)
     (l1 :accessor cpu-cycles-l1))
    (:documentation "Samples SB-IMPL::ELAPSED-CYCLES."))

  (defmethod start ((metric cpu-cycles))
    (multiple-value-bind (h0 l0) (sb-impl::read-cycle-counter)
      (setf (cpu-cycles-h0 metric) h0
            (cpu-cycles-l0 metric) l0)))

  (defmethod stop ((metric cpu-cycles))
    (multiple-value-bind (h1 l1) (sb-impl::read-cycle-counter)
      (setf (cpu-cycles-h1 metric) h1
            (cpu-cycles-l1 metric) l1)))

  (defmethod discard ((metric cpu-cycles))
    (setf (cpu-cycles-h0 metric) NIL
          (cpu-cycles-l0 metric) NIL
          (cpu-cycles-h1 metric) NIL
          (cpu-cycles-l1 metric) NIL))

  (defmethod commit ((metric cpu-cycles))
    (let ((cycles (sb-impl::elapsed-cycles
                   (cpu-cycles-h0 metric) (cpu-cycles-l0 metric)
                   (cpu-cycles-h1 metric) (cpu-cycles-l1 metric))))
      (when (<= 0 cycles)
        (vector-push-extend cycles (samples metric))))))

#+ecl
(progn
  (define-delta-metric (bytes-consed)
    "Samples GC_get_total_bytes."
    (ffi:c-inline () () :object "ecl_make_unsigned_integer(GC_get_total_bytes())" :one-liner t)))
