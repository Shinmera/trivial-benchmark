(in-package #:org.shirakumo.trivial-benchmark)

(defvar *default-samplers* ())

(defclass sampler () ())
(defgeneric variables (sampler))
(defgeneric wrap-measurement-form (sampler form))
(defgeneric commit-samples-form (sampler commit-fn))

(defun normalize-varspec (varspec)
  (destructuring-bind (name &optional (default 0) (type 'unsigned-byte)) (enlist varspec)
    (list (gensym (string name)) default type)))

(defmacro define-sampler (name vars &body forms)
  `(progn
     (defclass ,name (sampler)
       ((variables :reader variables :initform
                   (mapcar #'normalize-varspec ',vars))))

     ,@(loop for (type args . body) in forms
             collect (ecase type
                       (:measure
                        `(defmethod wrap-measurement-form ((,name ,name) ,@args)
                           (destructuring-bind ,(mapcar #'unlist vars) (mapcar #'unlist (variables ,name))
                             (declare (ignorable ,@(mapcar #'unlist vars)))
                             ,@body)))
                       (:commit
                        `(defmethod commit-samples-form ((,name ,name) ,@args)
                           (destructuring-bind ,(mapcar #'unlist vars) (mapcar #'unlist (variables ,name))
                             (declare (ignorable ,@(mapcar #'unlist vars)))
                             ,@body)))))))

(defmacro define-delta-sampler (name &body sample-point-forms)
  (destructuring-bind (name &optional (units 1)) (if (listp name) name (list name))
    (let ((form `(progn ,@sample-point-forms)))
      `(define-sampler ,name (var)
         (:measure (form)
                   (let ((start (gensym ,(format NIL "~a-~a" (string name) (string "START")))))
                     `(let ((,start ,',form))
                        (multiple-value-prog1
                            ,form
                          (setf ,var (- ,',form ,start))))))
         (:commit (commit-fn)
                  `(,commit-fn ,',name (/ (float ,var 0d0) ,',units)))))))

(define-delta-sampler (real-time internal-time-units-per-second)
  (get-internal-real-time))

(define-delta-sampler (run-time internal-time-units-per-second)
  (get-internal-run-time))

#+sbcl
(progn
  (define-sampler system-info (user-run-time system-run-time page-faults)
    (:measure (form)
              `(multiple-value-bind (a0 b0 c0) (sb-sys:get-system-info)
                 (multiple-value-prog1 ,form
                   (multiple-value-bind (a1 b1 c1) (sb-sys:get-system-info)
                     (setf ,user-run-time (- a1 a0) 
                           ,system-run-time (- b1 b0)
                           ,page-faults (- c1 c0))))))
    (:commit (commit-fn)
             `(,commit-fn user-run-time (/ (float ,user-run-time 0d0) 1000000)
                          system-run-time (/ (float ,system-run-time 0d0) 1000000)
                          page-faults ,page-faults)))

  (define-delta-sampler (gc-run-time 1000)
    sb-ext:*gc-run-time*)
  
  (define-delta-sampler bytes-consed
    (sb-ext:get-bytes-consed))

  (define-delta-sampler eval-calls
    sb-kernel:*eval-calls*)

  (define-sampler cycle-counter (h0 l0 h1 l1)
    (:measure (form)
              `(progn (multiple-value-setq (,h0 ,l0) (sb-impl::read-cycle-counter))
                      (multiple-value-prog1 ,form
                        (multiple-value-setq (,h1 ,l1) (sb-impl::read-cycle-counter)))))
    (:commit (commit-fn)
             `(,commit-fn cpu-cycles (sb-impl::elapsed-cycles ,h0 ,l0 ,h1 ,l1))))

  (define-sampler sb-time (user-run-time-us
                           system-run-time-us
                           real-time-ms
                           gc-run-time-ms
                           gc-real-time-ms
                           processor-cycles
                           eval-calls
                           lambdas-converted
                           page-faults
                           bytes-consed)
    (:measure (form)
              `(sb-ext:call-with-timing
                (lambda (&rest args)
                  (setf ,user-run-time-us (getf args :user-run-time-us 0)
                        ,system-run-time-us (getf args :system-run-time-us 0)
                        ,real-time-ms (getf args :real-time-ms 0)
                        ,gc-run-time-ms (getf args :gc-run-time-ms 0)
                        ,gc-real-time-ms (getf args :gc-real-time-ms 0)
                        ,processor-cycles (getf args :processor-cycles 0)
                        ,eval-calls (getf args :eval-calls 0)
                        ,lambdas-converted (getf args :lambdas-converted 0)
                        ,page-faults (getf args :page-faults 0)
                        ,bytes-consed (getf args :bytes-consed 0)))
                (lambda () ,form)))
    (:commit (commit-fn)
             `(,commit-fn user-run-time (/ ,user-run-time-us 1000000)
                          system-run-time (/ ,system-run-time-us 1000000)
                          real-time (/ ,real-time-ms 1000)
                          gc-run-time (/ ,gc-run-time-ms 1000)
                          gc-real-time (/ ,gc-real-time-ms 1000)
                          processor-cycles ,processor-cycles
                          eval-calls ,eval-calls
                          lambdas-converted ,lambdas-converted
                          page-faults ,page-faults
                          bytes-consed ,bytes-consed))))

#+ecl
(progn
  (define-delta-sampler (bytes-consed)
    (ffi:c-inline () () :object "ecl_make_unsigned_integer(GC_get_total_bytes())" :one-liner t)))
