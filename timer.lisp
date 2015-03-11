#|
 This file is a part of Trivial-Benchmark
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trivial-benchmark)

(defvar *default-metrics*
  ()
  "The list of class-names used to populate a TIMER by default.")

(defvar *default-computations*
  '(:count :total :minimum :maximum :median :average :deviation)
  "The list of computation-names used to print the REPORT table.")

(defclass metric ()
  ((running :initarg :running :initform NIL :accessor running))
  (:documentation "A class container for sampling information."))

(defgeneric running (metric)
  (:documentation "Returns T if the metric is currently sampling.

See START
See DISCARD
See COMMIT"))

(defmethod print-object ((metric metric) stream)
  (print-unreadable-object (metric stream :type T)
    (format stream "~s ~a" :size (sample-size metric))))

(defgeneric start (metric)
  (:documentation "Begin a sample for METRIC. 
Sets RUNNING to T.")
  (:method :around ((metric metric))
    (unless (running metric)
      (call-next-method)
      (setf (running metric) T))))

(defgeneric discard (metric)
  (:documentation "Stop and discard the current sample of METRIC. 
Sets RUNNING to NIL.")
  (:method :around ((metric metric))
    (when (running metric)
      (call-next-method)
      (setf (running metric) NIL))))

(defgeneric commit (metric)
  (:documentation "Stop and commit the current sample of METRIC. 
Sets RUNNING to NIL.")
  (:method :around ((metric metric))
    (when (running metric)
      (call-next-method)
      (setf (running metric) NIL))))

(defgeneric take-sample (metric)
  (:documentation "Return a current sampling value for METRIC.

Note that not all metrics must implement a method for this function.
It is perfectly plausible for a metric to skip this method if it
cannot provide a sample value at any point in time."))

(defgeneric samples (metric)
  (:documentation "Return a sequence of committed samples stored in METRIC."))

(defgeneric sample-size (metric)
  (:documentation "Return the number of samples stored in METRIC.")
  (:method (metric)
    (length (samples metric))))

(defgeneric condense (sample)
  (:documentation "Turn the SAMPLE value into a usable number.")
  (:method (thing)
    thing))

(defgeneric reduce-samples (metric function)
  (:documentation "Apply FUNCTION to the samples stored in METRIC in a REDUCE fashion.")
  (:method (metric function)
    (reduce function (samples metric) :key #'condense)))

(defgeneric compute (thing metric)
  (:documentation "Compute a value of the statistical computation THING for METRIC based on its current samples.")
  (:method ((x (eql :count)) (metric metric))
    (sample-size metric))
  (:method ((x (eql :total)) (metric metric))
    (reduce-samples metric #'+))
  (:method ((x (eql :minimum)) (metric metric))
    (reduce-samples metric #'min))
  (:method ((x (eql :maximum)) (metric metric))
    (reduce-samples metric #'max))
  (:method ((x (eql :median)) (metric metric))
    (elt (sort (copy-seq (samples metric)) #'<)
         (1- (ceiling (/ (compute :count metric) 2)))))
  (:method ((x (eql :average)) (metric metric))
    (/ (compute :total metric)
       (compute :count metric)))
  (:method ((x (eql :deviation)) (metric metric))
    (let ((metrics (samples metric))
          (average (compute :average metric)))
      (sqrt
       (/ (reduce #'+ (map (type-of metrics)
                           (lambda (a) (expt (- a average) 2))
                           metrics))
          (compute :count metric)))))
  (:method ((computations list) (metric metric))
    (mapcar (lambda (thing) (compute thing metric)) computations)))

(defgeneric report (metric &key stream computations)
  (:documentation "Print a report of all COMPUTATIONS for METRIC to STREAM")
  (:method ((metric metric) &key (stream T) (computations *default-computations*))
    (print-table
     (cons (list :computation :value)
           (loop for comp in computations
                 collect (list comp (compute comp metric))))
     :stream stream)))

(defgeneric reset (metric)
  (:documentation "Reset the METRIC and remove all its samples."))

(defclass timer ()
  ((metrics :initarg :metrics :accessor metrics))
  (:documentation "Class container for a set of METRICS."))

(defgeneric metrics (timer)
  (:documentation "Returns a list of metrics stored in TIMER."))

(defgeneric metric (type timer)
  (:documentation "Returns the metric of TYPE in TIMER if any.
The metric must match the type by TYPE=")
  (:method ((type symbol) (timer timer))
    (find type (metrics timer) :test #'type=)))

(defgeneric (setf metric) (metric timer)
  (:documentation "Sets the METRIC in TIMER.
The metric is replaced if it is found in the timer by TYPE= comparison.")
  (:method ((metric metric) (timer timer))
    (let ((pos (position metric (metrics timer) :test #'type=)))
      (if pos
          (setf (nth pos (metrics timer)) metric)
          (push metric (metrics timer))))))

(defgeneric metric-types (timer)
  (:documentation "Returns the types of metrics in TIMER.")
  (:method ((timer timer))
    (mapcar #'type-of (metrics timer))))

(defmethod initialize-instance :after ((timer timer) &key)
  (unless (slot-boundp timer 'metrics)
    (setf (metrics timer) ())
    (loop for type in *default-metrics*
          for metric = (make-instance type)
          do (setf (metric timer) metric))))

(defmethod print-object ((timer timer) stream)
  (print-unreadable-object (timer stream :type T)
    (format stream "~{~a~^ ~}" (metric-types timer))))

(defun make-timer (&optional (metric-types *default-metrics*))
  "Creates a TIMER object using the given METRIC-TYPES"
  (let ((*default-metrics* metric-types))
    (make-instance 'timer)))

(defun format-timer-stats (stream timer &optional (computations *default-computations*))
  (print-table
   (cons (cons :- computations)
         (loop for metric in (metrics timer)
               collect (list* (type-of metric)
                              (mapcar (lambda (a)
                                        (typecase a
                                          (fixnum (format NIL "~d" a))
                                          (T (format NIL "~f" (round-to a 6)))))
                                      (compute computations metric)))))
   :stream stream))

(defmethod describe-object ((timer timer) stream)
  (let ((*print-pretty* T))
    (format stream "This is an object for keeping benchmarking data.")
    (format stream "~&~%It tracks the following metric types:")
    (pprint-indent :block 2 stream)
    (format stream "~&~{~a~^, ~}" (metric-types timer))
    (terpri stream)
    (format stream "~&~%The statistics for the timer are:~&")
    (report timer :stream stream)))

(defun map-metrics (timer function)
  "Maps the metrics in TIMER, calling FUNCTION with each."
  (dolist (metric (metrics timer))
    (funcall function metric)))

(defmacro do-metrics ((metric-var timer &optional result-form) &body forms)
  "Binds METRIC-VAR to each metric of TIMER and then evaluates FORMS. 
Returns the value of RESULT-FORM after the loop."
  `(progn (map-metrics ,timer (lambda (,metric-var) ,@forms))
          ,result-form))

(defmethod take-sample ((timer timer))
  (map-metrics timer #'take-sample))

(defmethod start ((timer timer))
  (map-metrics timer #'start))

(defmethod discard ((timer timer))
  (map-metrics timer #'discard))

(defmethod commit ((timer timer))
  (map-metrics timer #'commit))

(defmethod report ((timer timer) &key (stream T) (computations *default-computations*))
  (if (< 0 (reduce #'max (mapcar #'sample-size (metrics timer))))
      (format-timer-stats stream timer computations)
      (format stream "No metric has any samples yet.")))

(defmethod reset ((timer timer))
  (map-metrics timer #'reset))

(defmacro with-sampling ((timer-form) &body forms)
  "Takes a sample for the evaluation time of FORMS and stores it in the timer given by TIMER-FORM.
Acts like a PROGN.

Specifically, START is called, then FORMS are evaluated. If an error occurs within the body,
DISCARD is called on the timer, otherwise COMMIT is called once the body exits.

See START
See DISCARD
See COMMIT"
  (let ((timer (gensym "TIMER"))
        (errord (gensym "ERRORD")))
    `(let ((,timer ,timer-form)
           (,errord NIL))
       (start ,timer)
       (unwind-protect
            (handler-bind ((error (lambda (err)
                                    (declare (ignore err))
                                    (setf ,errord T)
                                    (discard ,timer))))
              ,@forms)
         (unless ,errord
           (commit ,timer))))))

(defmacro with-timing ((n &optional (timer-form '(make-timer)) (stream T) (computations '*default-computations)) &body forms)
  "Evaluates FORMS N times, using WITH-SAMPLING on the value of TIMER-FORM each iteration.
At the end, runs REPORT on the timer with STREAM and COMPUTATIONS.

See WITH-SAMPLING"
  (let ((timer (gensym "TIMER")))
    `(let ((,timer ,timer-form))
       (loop repeat ,n
             do (with-sampling (,timer)
                  ,@forms))
       (report ,timer :stream ,stream :computations ,computations)
       NIL)))
