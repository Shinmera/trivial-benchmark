(in-package #:org.shirakumo.trivial-benchmark)

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
                 (pop sample-point-forms)))
          (form `(progn ,@sample-point-forms)))
      `(progn
         (pushnew ',name *default-metrics*)
         
         (defmethod wrap-measurement-form ((metric (eql ',name)) form var)
           ,doc
           (let ((start (gensym ,(format NIL "~a-~a" (string name) (string "START")))))
             (values `(let ((,start ,',form))
                        (multiple-value-prog1
                            ,form
                          (setf ,var (- ,',form ,start))))
                     (when (/= 1 ,units) `(/ (float ,var 0d0) ,',units)))))))))

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
    sb-ext:*gc-run-time*)
  
  (define-delta-metric bytes-consed
    "Samples SB-IMPL::GET-BYTES-CONSED."
    (sb-ext:get-bytes-consed))

  (define-delta-metric eval-calls
    "Samples SB-IMPL::*EVAL-CALLS*."
    sb-kernel:*eval-calls*)

  (pushnew 'cpu-cycles *default-metrics*)
  (defmethod wrap-measurement-form ((metric (eql 'cpu-cycles)) form var)
    (let ((h0 (gensym "H0")) (l0 (gensym "L0"))
          (h1 (gensym "H1")) (l1 (gensym "L1")))
      `(multiple-value-bind (,h0 ,l0) (sb-impl::read-cycle-counter)
         (multiple-value-prog1 ,form
           (multiple-value-bind (,h1 ,l1) (sb-impl::read-cycle-counter)
             (setf ,var (sb-impl::elapsed-cycles ,h0 ,l0 ,h1 ,l1))))))))

#+ecl
(progn
  (define-delta-metric (bytes-consed)
    "Samples GC_get_total_bytes."
    (ffi:c-inline () () :object "ecl_make_unsigned_integer(GC_get_total_bytes())" :one-liner t)))
