(defsystem trivial-benchmark
  :name "Trivial-Benchmark"
  :version "3.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An easy to use benchmarking system."
  :homepage "https://shinmera.github.io/trivial-benchmark/"
  :bug-tracker "https://github.com/Shinmera/trivial-benchmark/issues"
  :source-control (:git "https://github.com/Shinmera/trivial-benchmark.git")
  :depends-on (#:alexandria)
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "timer")
               (:file "samples")
               (:file "suite")))
