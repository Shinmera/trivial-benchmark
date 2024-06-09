(in-package #:org.shirakumo.trivial-benchmark)

(defvar *default-computations* '(:samples :total :minimum :maximum :median :average :deviation))
(defvar *default-metrics* ())

(defgeneric compute (thing samples))
(defgeneric report-to (stream thing &key))
(defgeneric samples (timer metric))
(defgeneric metric-types (timer))
(defgeneric reset (timer))

(defun report (thing &rest args &key (stream T) &allow-other-keys)
  (remf args :stream)
  (apply #'report-to stream thing args))

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

(defmethod samples ((timer timer) metric)
  (or (gethash metric (metrics timer))
      (setf (gethash metric (metrics timer)) (make-array 1024 :adjustable T :fill-pointer 0))))

(defmethod metric-types ((timer timer))
  (loop for key being the hash-keys of (metrics timer) collect key))

(defun format-timer-stats (stream timer &key (computations *default-computations*)
                                             (metrics *default-metrics*))
  (print-table
   (cons (cons :- computations)
         (loop for metric in (or metrics
                                 (loop for k being the hash-keys of (metrics timer) collect k))
               for samples = (samples timer metric)
               when (< 0 (length samples))
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

(defmethod report-to ((stream stream) (timer timer) &rest args &key &allow-other-keys)
  (if (loop for samples being the hash-values of (metrics timer)
            thereis (< 0 (length samples)))
      (apply #'format-timer-stats stream timer args)
      (format stream "No metric has any samples yet."))
  timer)

(defmethod reset ((timer timer))
  (loop for samples being the hash-values of (metrics timer)
        do (setf (fill-pointer samples) 0))
  timer)

(defmacro with-sampling ((timer-form &rest samplers) &body forms)
  (let* ((timer (gensym "TIMER"))
         (commit-fn (gensym "COMMIT"))
         (samplers (loop for sampler in (or samplers *default-samplers*)
                         collect (make-instance sampler)))
         (form `(progn ,@forms))
         (vars (loop for sampler in samplers
                     append (variables sampler))))
    (loop for sampler in samplers
          do (setf form (wrap-measurement-form sampler form)))
    `(let ((,timer ,timer-form)
           ,@(loop for var in vars
                   collect `(,(first var) ,(second var))))
       (declare ,@(loop for var in vars
                        collect `(type ,(third var) ,(first var))))
       (multiple-value-prog1
           ,form
         (macrolet ((,commit-fn (&rest pairs)
                      `(progn ,@(loop for (metric sample) on pairs by #'cddr
                                      collect `(vector-push-extend (float ,sample 0f0) (samples ,',timer ',metric))))))
           ,@(loop for sampler in samplers
                   collect (commit-samples-form sampler commit-fn)))))))

(defmacro with-timing ((n &key ((:timer timer-form) '(make-instance 'timer))
                               (stream T)
                               (samplers *default-samplers*)
                               (metrics '*default-metrics*)
                               (computations '*default-computations*))
                       &body forms)
  (let ((timer (gensym "TIMER")))
    `(let ((,timer ,timer-form))
       (loop repeat ,n
             do (with-sampling (,timer ,@samplers)
                  ,@forms))
       (report ,timer :stream ,stream
                      :metrics ,metrics
                      :computations ,computations))))
