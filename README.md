About Trivial-Benchmark
-----------------------
Frequently I want to do a quick benchmark comparison of my functions. `TIME` is nice to get some data, but it's limited to a single run so there isn't really much of a statistical value in it. Trivial-benchmark runs a block of code many times and outputs some statistical data for it. On SBCL this includes the data from `TIME`, for all other implementations just the REAL- and RUN-TIME data.

How To
------
```
(benchmark:with-timing 1000
  (something-to-test))
```

Sample:

![Output Screenshot](http://shinmera.tymoon.eu/public/plump-benchmark.png)
