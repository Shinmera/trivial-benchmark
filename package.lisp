(defpackage #:org.shirakumo.trivial-benchmark
  (:nicknames #:trivial-benchmark)
  (:use #:cl)
  ;; toolkit.lisp
  (:export
   #:print-table)
  ;; sampler.lisp
  (:export
   #:*default-samplers*
   #:sampler
   #:variables
   #:wrap-measurement-form
   #:commit-samples-form
   #:define-sampler
   #:define-delta-sampler
   #:real-time
   #:run-time
   #:system-info
   #:gc-run-time
   #:bytes-consed
   #:eval-calls
   #:cycle-counter
   #:sb-time
   #:user-run-time
   #:system-run-time
   #:real-time
   #:gc-run-time
   #:gc-real-time
   #:processor-cycles
   #:eval-calls
   #:lambdas-converted
   #:page-faults
   #:bytes-consed)
  ;; timer.lisp
  (:export
   #:*default-computations*
   #:*default-metrics*
   #:compute
   #:report-to
   #:samples
   #:metric-types
   #:reset
   #:report
   #:timer
   #:format-timer-stats
   #:reset
   #:with-sampling
   #:with-timing))

