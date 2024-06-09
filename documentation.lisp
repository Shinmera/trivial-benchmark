(in-package #:org.shirakumo.trivial-benchmark)

;; toolkit.lisp
(docs:define-docs
  (function print-table
    ""))

;; sampler.lisp
(docs:define-docs
  (variable *default-samplers*
    "")

  (type sampler
    "")

  (function variables
    "")

  (function wrap-measurement-form
    "")

  (function commit-samples-form
    "")

  (function define-sampler
    "")

  (function define-delta-sampler
    "")

  (function real-time
    "")

  (function run-time
    "")

  (function system-info
    "")

  (function gc-run-time
    "")

  (function bytes-consed
    "")

  (function eval-calls
    "")

  (function cycle-counter
    "")

  (function sb-time
    "")

  (function user-run-time
    "")

  (function system-run-time
    "")

  (function real-time
    "")

  (function gc-run-time
    "")

  (function gc-real-time
    "")

  (function processor-cycles
    "")

  (function eval-calls
    "")

  (function lambdas-converted
    "")

  (function page-faults
    "")

  (function bytes-consed
    ""))

;; timer.lisp
(docs:define-docs
  (variable *default-computations*
    "")

  (variable *default-metrics*
    "")

  (function compute
    "")

  (function report-to
    "")

  (function samples
    "")

  (function metric-types
    "")

  (function report
    "")

  (type timer
    "")

  (function format-timer-stats
    "")

  (function reset
    "")

  (function with-sampling
    "")

  (function with-timing
    ""))
