#|
 This file is a part of Trivial-Benchmark
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Robert Smith <quad@symbo1ics.com>
|#

(in-package #:org.shirakumo.trivial-benchmark)

(defvar *benchmark-packages* (make-hash-table :test 'eq)
  "A table mapping package objects (which are benchmark packages) to a list of fbound symbols.")

(defun find-benchmark-package (package-designator)
  "Find the benchmark package designated by PACKAGE-DESIGNATOR and return it, otherwise return NIL if it doesn't exist."
  (let ((package-object (find-package package-designator)))
    (if (or (null package-object)
            (not (nth-value 1 (gethash package-object *benchmark-packages*))))
        nil
        package-object)))

(defmethod report-to ((stream stream) (hash-table hash-table) &key (computations *default-computations*))
  (dolist (result (alexandria:hash-table-alist hash-table))
    (destructuring-bind (name &rest metrics) result
      (format stream "Results for benchmark ~a~%" (string name))
      (print-table (cons (cons :- computations)
                         (loop :for metric :in metrics
                               :collect (remove-if (lambda (elt) (find elt computations))
                                                   metric))))
      (format stream "~%"))))

(defmacro define-benchmark-package (name &body package-options)
  "Define a package as if by DEFPACKAGE, except that both the COMMON-LISP and TRIVIAL-BENCHMARK packages are used, and that this package is designated as one containing benchmarks."
  `(progn
     (defpackage ,name
       (:use #:trivial-benchmark #:cl)
       ,@package-options)
     (setf (gethash (find-package ',name) *benchmark-packages*) nil)
     (find-package ',name)))

(defvar *current-suite-report*)         ; Table mapping fn name -> metric alist
(defvar *current-benchmark*)            ; Name of current benchmark
(defvar *current-timer*)                ; Timer used in the current benchmark.

(defmacro with-benchmark-sampling (&body forms)
  "Benchmark the execution of the forms FORMS within the context of a DEFINE-BENCH macro. Acts like PROGN."
  `(with-sampling (*current-timer*)
     ,@forms))

(defun metrics-alist (timer &optional (computations *default-computations*))
  (loop :for metric :in (metrics timer)
        :collect (cons (type-of metric)
                       (mapcan #'list
                               computations
                               (compute computations metric)))))

(defmacro define-benchmark (name args &body body)
  "Define a benchmark named NAME. The structure of this macro is that of DEFUN, except that WITH-BENCHMARK-SAMPLING can be called to collect metrics on the enclosed forms.

ARGS must be a lambda list that does not require arguments present at the call site."
  (check-type name symbol)
  (let ((benchmark-name name))
    (multiple-value-bind (forms decls doc)
        (alexandria:parse-body body :documentation t)
      `(progn
         (defun ,benchmark-name ,args
           ,@(alexandria:ensure-list doc)
           ,@decls
           (let ((*current-timer*     (benchmark:make-timer))
                 (*current-benchmark* ',benchmark-name))
             (unwind-protect (progn ,@forms)
               (when (boundp '*current-suite-report*)
                 (setf (gethash ',benchmark-name *current-suite-report*)
                       (metrics-alist *current-timer*))))))
         (pushnew ',benchmark-name
                  (gethash (find-benchmark-package
                            (symbol-package ',benchmark-name))
                           *benchmark-packages*))
         ',benchmark-name))))

(defun make-suite-report-table ()
  (make-hash-table :test 'eq))

(defun run-package-benchmarks (&key (package *package*)
                                    (verbose nil))
  "Run all of the benchmarks associated with the benchmark package PACKAGE, the current one by default.

Return a hash table mapping benchmark names to their metrics represented as lists. In particular, it will be an alist whose CARs are metrics and whose CDRs are plists of computations.

VERBOSE is a generalized boolean that controls output verbosity while testing.
"
  (let ((benchmark-package (find-benchmark-package package)))
    (when (null benchmark-package)
      (error "The benchmark package ~A isn't defined." package))
    (let ((*current-suite-report* (make-suite-report-table)))
      ;; Collect the report.
      (loop :for fn :in (gethash benchmark-package *benchmark-packages*)
            :do (when verbose
                  (format *trace-output* "~&Benchmarking ~S..." fn)
                  (force-output *trace-output*))
                (funcall fn)
                (when verbose
                  (format *trace-output* "done.~%")))
      ;; Return the results collected.
      *current-suite-report*)))

(defmacro with-suite-report ((suite) &body body)
  `(let* ((,suite (or (and (boundp '*current-suite-report*)
                           *current-suite-report*)
                      (make-suite-report-table)))
          (*current-suite-report* ,suite))
     ,@body))
