#|
 This file is a part of Trivial-Benchmark
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.trivial-benchmark.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.trivial-benchmark.asdf)

(defsystem trivial-benchmark
  :name "Trivial-Benchmark"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Some simple benchmarking helper functions"
  :serial T
  :components ((:file "benchmark")))
