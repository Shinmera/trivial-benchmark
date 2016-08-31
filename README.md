About Trivial-Benchmark
-----------------------

Frequently I want to do a quick benchmark comparison of my functions. `TIME` is nice to get some data, but it's limited to a single run so there isn't really much of a statistical value in it. Trivial-Benchmark runs a block of code many times and outputs some statistical data for it. On SBCL this includes the data from `TIME`, for all other implementations just the REAL- and RUN-TIME data. However, you can extend the system by adding your own `metric`s to it, or even by adding additional statistical `compute`ations. 

Measurement How-To
------------------

For basic throwaway benchmarking, the `with-timing` macro should suffice:

    (benchmark:with-timing (1000)
      (+ 1 1))

However, you can also do more complex timing using `make-timer` and `with-sampling`. The former creates a new timer object (with an optional list of metrics to sample) and the latter collects one sample for each metric of the timer for the duration of the body forms.

    (defvar *timer* (benchmark:make-timer))
    
    (benchmark:with-sampling (*timer*)
      (+ 1 1))
    
    (benchmark:with-sampling (*timer*)
      (expt 10 100))
    
    (benchmark:report *timer*)
    
    (benchmark:reset *timer*)
    
    (benchmark:report *timer*)

Sample Output:  
![Output Screenshot](https://filebox.tymoon.eu/file/TkRVdw==)

Extending Trivial-Benchmark
---------------------------
If you're interested in adding additional metrics, you'll want to take a look at the `metric` class, as well as the related methods, `start` `stop` `discard` `commit` `take-sample` `samples` `sample-size` `condense` `reduce-samples` `compute` `report` `reset`. For a basic `metric`, you only need to implement `start`, `stop`, `commit`, `samples`, and `take-sample`. The other functions have standard methods that will do their computations based on those five.

If you have a function that returns a current state of your sample and simply want to have the metric show the delta between `start` and `stop`, you can use `define-delta-metric`.

    (define-delta-metric (run-time internal-time-units-per-second)
      (get-internal-run-time))

You can also implement a new computation type that is used to display the different metrics in the table. For that, simply write a method on `compute` with the name of your computation as an eql-specializer and then push the name onto `*default-computations*` so that it's always displayed when using `report`. As an example, the maximum is simply calculated as follows:

    (defmethod compute ((x (eql :maximum)) (metric metric))
      (reduce-samples metric #'max))

Benchmark Suites
----------------

Analogous to unit testing, *performance testing* or *benchmark testing* is a useful way to measure performance regressions in a code base. Trivial-Benchmark has **preliminary support** for defining and running benchmarks.

First, a *benchmark package* needs to be defined, which is essentially a normal Common Lisp package with some additional bookkeeping.

    (define-benchmark-package #:my-app-benchmarks
      (:use #:my-app)
      (:export #:run-benchmarks))

The macro `define-benchmark-package` automatically uses the `COMMON-LISP` and the `TRIVIAL-BENCHMARK` packages.

After the package has been defined, you can define benchmarks like normal Lisp function definitions using the `define-benchmark` macro. Within the `define-benchmark` macro, you use `with-benchmark-sampling` around forms that you wish to measure.

    (define-benchmark measure-trig ()
      (declare (optimize speed))
      (loop :repeat 100000
            :for r := (random 1.0d0)
            :do (with-benchmark-sampling
                  (+ (sin r)
                     (cos r)
                     (tan r)))))

The macro `with-benchmark-sampling` can be called many times throughout the body of the `define-benchmark`, and even called within functions called within the body. In other words, you're able to write something like this:

    (defun trig-trial (r)
      (with-benchmark-sampling
        (+ (sin r) (cos r) (tan r))))

    (defun inv-trig-trial (r)
      (with-benchmark-sampling
        (+ (asin r) (acos r) (atan r))))
    
    (define-benchmark measure-all-trig ()
      (declare (optimize speed))
      (loop :repeat 100000
            :for r := (random 1.0d0)
            :do (trig-trial r)
                (inv-trig-trial r)))

You can run the benchmarks of a package by doing:

    (run-package-benchmarks :package ':my-app-benchmarks
                            :verbose t)

You can elide the `:package` argument if you're in the package already. Running this will give:

    > (run-package-benchmarks :package ':my-app-benchmarks 
                              :verbose t)
    Benchmarking MEASURE-ALL-TRIG...done.
    Benchmarking BENCH-TRIG...done.
    #<HASH-TABLE :TEST EQ :COUNT 2 {1002FD8243}>

The returned hash table contains a summary of all of the statistics, which you can further process as needed (e.g., record, plot, etc.). The table maps the benchmark names to an alist of metrics and their computed statistics. We can inspect this easily enough:

```
> (alexandria:hash-table-alist *)
((BENCH-TRIG
  (REAL-TIME :SAMPLES 100000 :TOTAL 52/125 :MINIMUM 0 :MAXIMUM 3/1000 :MEDIAN 0
   :AVERAGE 13/3125000 :DEVIATION 6.482819e-5)
  (RUN-TIME :SAMPLES 100000 :TOTAL 383/1000 :MINIMUM 0 :MAXIMUM 1/1000 :MEDIAN
   0 :AVERAGE 383/100000000 :DEVIATION 6.176837e-5)
  (USER-RUN-TIME :SAMPLES 100000 :TOTAL 52663/250000 :MINIMUM 1/1000000
   :MAXIMUM 53/1000000 :MEDIAN 1/500000 :AVERAGE 52663/25000000000 :DEVIATION
   9.770944e-7)
  (SYSTEM-RUN-TIME :SAMPLES 100000 :TOTAL 94299/500000 :MINIMUM 1/1000000
   :MAXIMUM 87/1000000 :MEDIAN 1/500000 :AVERAGE 94299/50000000000 :DEVIATION
   1.1286627e-6)
  (PAGE-FAULTS :SAMPLES 100000 :TOTAL 0 :MINIMUM 0 :MAXIMUM 0 :MEDIAN 0
   :AVERAGE 0 :DEVIATION 0.0)
  (GC-RUN-TIME :SAMPLES 100000 :TOTAL 0 :MINIMUM 0 :MAXIMUM 0 :MEDIAN 0
   :AVERAGE 0 :DEVIATION 0.0)
  (BYTES-CONSED :SAMPLES 100000 :TOTAL 0 :MINIMUM 0 :MAXIMUM 0 :MEDIAN 0
   :AVERAGE 0 :DEVIATION 0.0)
  (EVAL-CALLS :SAMPLES 100000 :TOTAL 0 :MINIMUM 0 :MAXIMUM 0 :MEDIAN 0 :AVERAGE
   0 :DEVIATION 0.0))
 (MEASURE-ALL-TRIG
  (REAL-TIME :SAMPLES 200000 :TOTAL 102/125 :MINIMUM 0 :MAXIMUM 1/200 :MEDIAN 0
   :AVERAGE 51/12500000 :DEVIATION 6.4601496e-5)
  (RUN-TIME :SAMPLES 200000 :TOTAL 77/100 :MINIMUM 0 :MAXIMUM 1/1000 :MEDIAN 0
   :AVERAGE 77/20000000 :DEVIATION 6.192881e-5)
  (USER-RUN-TIME :SAMPLES 200000 :TOTAL 41371/100000 :MINIMUM 1/1000000
   :MAXIMUM 1/12500 :MEDIAN 1/500000 :AVERAGE 41371/20000000000 :DEVIATION
   1.2130914e-6)
  (SYSTEM-RUN-TIME :SAMPLES 200000 :TOTAL 363543/1000000 :MINIMUM 1/1000000
   :MAXIMUM 11/100000 :MEDIAN 1/500000 :AVERAGE 363543/200000000000 :DEVIATION
   1.4713012e-6)
  (PAGE-FAULTS :SAMPLES 200000 :TOTAL 0 :MINIMUM 0 :MAXIMUM 0 :MEDIAN 0
   :AVERAGE 0 :DEVIATION 0.0)
  (GC-RUN-TIME :SAMPLES 200000 :TOTAL 0 :MINIMUM 0 :MAXIMUM 0 :MEDIAN 0
   :AVERAGE 0 :DEVIATION 0.0)
  (BYTES-CONSED :SAMPLES 200000 :TOTAL 15106048 :MINIMUM 0 :MAXIMUM 131072
   :MEDIAN 0 :AVERAGE 236032/3125 :DEVIATION 1595.1276)
  (EVAL-CALLS :SAMPLES 200000 :TOTAL 0 :MINIMUM 0 :MAXIMUM 0 :MEDIAN 0 :AVERAGE
   0 :DEVIATION 0.0)))
```