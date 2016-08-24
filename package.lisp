#|
 This file is a part of Trivial-Benchmark
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:trivial-benchmark
  (:use #:cl)
  (:nicknames #:org.shirakumo.trivial-benchmark #:benchmark)
  ;; samples.lisp
  (:export
   #:vector-metric
   #:delta-metric
   #:starting-value
   #:define-delta-metric
   #:real-time
   #:run-time)
  #+sbcl
  (:export
   #:user-run-time
   #:system-run-time
   #:page-faults
   #:gc-run-time
   #:bytes-consed
   #:eval-calls
   #:cpu-cycles)
  ;; timer.lisp
  (:export
   #:*default-metrics*
   #:*default-computations*
   #:metric
   #:running
   #:start
   #:discard
   #:commit
   #:take-sample
   #:samples
   #:sample-size
   #:condense
   #:reduce-samples
   #:compute
   #:report
   #:reset
   #:timer
   #:metrics
   #:metric
   #:metric-types
   #:make-timer
   #:map-metrics
   #:do-metrics
   #:with-sampling
   #:with-timing)
  ;; toolkit.lisp
  (:export
   #:type=
   #:print-table
   #:round-to)
  ;; suite.lisp
  (:export
   #:find-benchmark-package
   #:define-benchmark-package
   #:define-benchmark
   #:with-benchmark-sampling
   #:run-package-benchmarks))

