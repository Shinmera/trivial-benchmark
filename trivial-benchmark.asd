(defsystem trivial-benchmark
  :name "Trivial-Benchmark"
  :version "3.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An easy to use benchmarking system."
  :homepage "https://shinmera.com/docs/trivial-benchmark/"
  :bug-tracker "https://shinmera.com/project/trivial-benchmark/issues"
  :source-control (:git "https://shinmera.com/project/trivial-benchmark.git")
  :depends-on (:documentation-utils)
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "sampler")
               (:file "timer")
               (:file "documentation")))
