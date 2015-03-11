#|
 This file is a part of Trivial-Benchmark
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.trivial-benchmark)

(defvar *default-metrics*
  ())

(defvar *default-computations*
  '(:count :total :minimum :maximum :median :average :deviation))

(defclass metric ()
  ((running :initarg :running :initform NIL :accessor running)))

(defmethod print-object ((metric metric) stream)
  (print-unreadable-object (metric stream :type T)
    (format stream "~s ~a" :size (sample-size metric))))

(defgeneric start (metric)
  (:method :around ((metric metric))
    (unless (running metric)
      (call-next-method)
      (setf (running metric) T))))

(defgeneric discard (metric)
  (:method :around ((metric metric))
    (when (running metric)
      (call-next-method)
      (setf (running metric) NIL))))

(defgeneric commit (metric)
  (:method :around ((metric metric))
    (when (running metric)
      (call-next-method)
      (setf (running metric) NIL))))

(defgeneric take-sample (metric))

(defgeneric samples (metric))

(defgeneric sample-size (metric)
  (:method (metric)
    (length (samples metric))))

(defgeneric condense (metric-point)
  (:method (thing)
    thing))

(defgeneric reduce-samples (metric function)
  (:method (metric function)
    (reduce function (samples metric) :key #'condense)))

(defgeneric compute (thing metric)
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
  (:method ((metric metric) &key (stream T) (computations *default-computations*))
    (print-table
     (cons (list :computation :value)
           (loop for comp in computations
                 collect (list comp (compute comp metric))))
     :stream stream)))

(defgeneric reset (metric))

(defclass timer ()
  ((metrics :initarg :metrics :accessor metrics)))

(defgeneric metric (type timer)
  (:method ((type symbol) (timer timer))
    (find type (metrics timer) :test (lambda (a b) (typep b a)))))

(defgeneric (setf metric) (metric timer)
  (:method ((metric metric) (timer timer))
    (pushnew metric (metrics timer) :test (lambda (a b) (typep b (type-of a))))))

(defgeneric metric-types (timer)
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

(defun make-timer (&optional (metric-types NIL stp))
  (let ((*default-metrics*
          (if stp
              metric-types
              *default-metrics*))))
  (make-instance 'timer))

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
    (pprint-logical-block (stream NIL)
      (format stream "~@:_It tracks the following metric types:")
      (pprint-indent :block 2 stream)
      (format stream "~@:_~<~;~{~s~^, ~:_~}~;~:>" (metric-types timer))
      (terpri stream))
    (pprint-logical-block (stream NIL)
      (format stream "~@:_The statistics for the timer are:")
      (format-timer-stats stream timer))))

(defun map-metrics (timer function)
  (dolist (metric (metrics timer))
    (funcall function metric)))

(defmacro do-metrics ((metric-var timer &optional result-form) &body forms)
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
  (format-timer-stats stream timer computations))

(defmethod reset ((timer timer))
  (map-metrics timer #'reset))

(defmacro with-sampling ((timer-form) &body forms)
  (let ((timer (gensym "TIMER")))
    `(let ((,timer ,timer-form))
       (unwind-protect
            (progn (start ,timer)
                   ,@forms)
         (commit ,timer)))))

(defmacro with-timing ((n &optional (timer-form '(make-timer))) &body forms)
  (let ((timer (gensym "TIMER"))
        (function (gensym "FUNCTION")))
    `(let ((,timer ,timer-form))
       (flet ((,function ()
                ,@forms))
         (loop repeat ,n
               do (with-sampling (,timer)
                    (,function))))
       (report ,timer))))
