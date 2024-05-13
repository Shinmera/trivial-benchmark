(in-package #:org.shirakumo.trivial-benchmark)

(defvar *default-metrics* ())
(defvar *default-computations* '(:samples :total :minimum :maximum :median :average :deviation))

(defgeneric wrap-measurement-form (metric form output-variable))
(defgeneric compute (thing samples))
(defgeneric report-to (stream thing &key))
(defgeneric samples (timer metric))
(defgeneric metric-types (timer))
(defgeneric reset (timer))

(defun report (thing &key (stream T) (computations *default-computations*))
  (report-to stream thing :computations computations))

(defmethod compute ((x (eql :count)) (samples vector))
  (length samples))

(defmethod compute ((x (eql :samples)) (samples vector))
  (length samples))

(defmethod compute ((x (eql :total)) (samples vector))
  (if (= 0 (length samples))
      :n/a
      (reduce #'+ samples)))

(defmethod compute ((x (eql :minimum)) (samples vector))
  (if (= 0 (length samples))
      :n/a
      (reduce #'min samples)))

(defmethod compute ((x (eql :maximum)) (samples vector))
  (if (= 0 (length samples))
      :n/a
      (reduce #'max samples)))

(defmethod compute ((x (eql :median)) (samples vector))
  (if (= 0 (length samples))
      :n/a
      (elt (sort (copy-seq samples) #'<)
           (1- (ceiling (/ (length samples) 2))))))

(defmethod compute ((x (eql :average)) (samples vector))
  (if (= 0 (length samples))
      :n/a
      (/ (reduce #'+ samples) (length samples))))

(defmethod compute ((x (eql :deviation)) (samples vector))
  (if (= 0 (length samples))
      :n/a
      (let ((average (compute :average samples)))
        (sqrt
         (/ (reduce #'+ samples :key (lambda (a) (expt (- a average) 2)))
            (length samples))))))

(defmethod compute ((computations sequence) samples)
  (map (type-of computations) (lambda (thing) (compute thing samples)) computations))

(defmethod report-to ((stream (eql T)) thing &rest args &key &allow-other-keys)
  (apply #'report-to *standard-output* thing args))

(defmethod report-to ((string (eql NIL)) thing &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'report-to stream thing args)))

(defmethod report-to ((stream stream) (samples vector) &key computations (padding 2))
  (print-table
   (cons (list :computation :value)
         (loop for comp in computations
               collect (list comp (compute comp samples))))
   :stream stream :padding padding))

(defclass timer ()
  ((metrics :initform (make-hash-table :test 'eql) :accessor metrics)))

(defmethod print-object ((timer timer) stream)
  (print-unreadable-object (timer stream :type T)
    (format stream "~{~a~^ ~}" (metric-types timer))))

(defmethod shared-initialize :after ((timer timer) slots &key (metrics *default-metrics*))
  (map NIL (lambda (metric)
             (unless (gethash metric (metrics timer))
               (setf (gethash metric (metrics timer))
                     (make-array 1024 :adjustable T :fill-pointer 0))))
       metrics))

(defmethod samples ((timer timer) metric)
  (or (gethash metric (metrics timer))
      (error "Timer does not store any samples for ~s" metric)))

(defmethod metric-types ((timer timer))
  (loop for key being the hash-keys of (metrics timer) collect key))

(defun format-timer-stats (stream timer &optional (computations *default-computations*))
  (print-table
   (cons (cons :- computations)
         (loop for metric being the hash-keys of (metrics timer) using (hash-value samples)
               collect (list* metric
                              (mapcar (lambda (a)
                                        (typecase a
                                          (symbol (format NIL "~a" a))
                                          (fixnum (format NIL "~d" a))
                                          (T (format NIL "~f" (round-to a 6)))))
                                      (compute computations samples)))))
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

(defmethod report-to ((stream stream) (timer timer) &key computations)
  (if (loop for samples being the hash-values of (metrics timer)
            thereis (< 0 (length samples)))
      (format-timer-stats stream timer computations)
      (format stream "No metric has any samples yet."))
  timer)

(defmethod reset ((timer timer))
  (loop for samples being the hash-values of (metrics timer)
        do (setf (fill-pointer samples) 0))
  timer)

(defmacro with-sampling ((timer-form &rest metrics) &body forms)
  (unless metrics
    (setf metrics *default-metrics*))
  (let ((timer (gensym "TIMER"))
        (vars (loop for metric in metrics
                    collect (gensym (princ-to-string metric))))
        (finalizers ())
        (form `(progn ,@forms)))
    (loop for metric in metrics
          for var in vars
          do (multiple-value-bind (wrapped-form finalizer)
                 (wrap-measurement-form metric form var)
               (setf form wrapped-form)
               (push (or finalizer var) finalizers)))
    `(let ((,timer ,timer-form)
           ,@(loop for var in vars
                   collect `(,var 0)))
       (declare (type unsigned-byte ,@vars))
       (multiple-value-prog1
           ,form
         ,@(loop for metric in metrics
                 for var in vars
                 for finalizer in (nreverse finalizers)
                 collect `(vector-push-extend ,finalizer (samples ,timer ',metric)))))))

(defmacro with-timing ((n &optional (timer-form '(make-instance 'timer)) (stream T) (computations '*default-computations*)) &body forms)
  (let ((timer (gensym "TIMER")))
    `(let ((,timer ,timer-form))
       (loop repeat ,n
             do (with-sampling (,timer)
                  ,@forms))
       (report ,timer :stream ,stream :computations ,computations))))
