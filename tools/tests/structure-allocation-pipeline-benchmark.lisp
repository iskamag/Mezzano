;;;; Load structure-allocation-benchmark.lisp before this file.
(in-package :cl-user)

;; The 8f25d879 allocation path, with its original policy and helper calls.
(defun allocate-struct-pipeline-baseline (class)
  (declare (optimize (speed 1) (safety 3) (debug 3))
           (notinline mezzano.runtime::structure-class-p mezzano.internals::%allocate-instance))
  (when (symbolp class) (setf class (find-class class)))
  (assert (mezzano.runtime::structure-class-p class))
  (let ((layout (mezzano.runtime::instance-access-by-name class 'mezzano.clos::slot-storage-layout)))
    (mezzano.internals::%allocate-instance layout)))

(defun pipeline-original-slot-scan (layout slot-name)
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  (let ((instance-slots (mezzano.internals::layout-instance-slots layout)))
    (loop for i below (mezzano.internals::%object-header-data instance-slots) by 2
          when (eq slot-name (mezzano.internals::%object-ref-t instance-slots i))
          do (return (mezzano.internals::%object-ref-t instance-slots (1+ i)))
          finally (error "Instance slot ~S missing from layout ~S" slot-name layout))))

(defun compare-allocation-pipeline (source &optional (depth 20) (pairs 4))
  (let* ((helper 'mezzano.runtime::%allocate-struct-known-class)
         (original-helper (symbol-function helper))
         (original-benchmark (and (fboundp 'gc-benchmark) (symbol-function 'gc-benchmark)))
         (generator 'mezzano.internals::generate-simple-defstruct-constructor)
         (original-generator (symbol-function generator))
         (slot-scan 'mezzano.runtime::slot-location-in-layout)
         (original-slot-scan (symbol-function slot-scan))
         (variants nil))
    (unwind-protect
         (progn
           (dolist (mode '(:baseline :inline))
             (setf (symbol-function generator)
                   (if (eq mode :inline) original-generator
                       (lambda (&rest args)
                         (let ((definition (apply original-generator args)))
                           (setf (caadr (second definition)) 'mezzano.internals::maybe-inline)
                           definition))))
             (proclaim `(,(if (eq mode :inline) 'inline 'notinline) ,helper))
             (let* ((package-name (format nil "MEZZANO.PIPELINE-BENCHMARK.~A" mode))
                    (*package* (or (find-package package-name)
                                   (make-package package-name :use '(:cl)))))
               (load (compile-file source
                                   :output-file (merge-pathnames
                                                 (format nil "allocation-pipeline-~A.llf" mode)
                                                 (user-homedir-pathname))))
               (push (cons mode (symbol-function (find-symbol "GC-BENCHMARK" *package*))) variants)))
           (setf (symbol-function generator) original-generator)
           (proclaim `(inline ,helper))
           (dotimes (pair pairs)
             (dolist (mode (if (evenp pair) '(:baseline :inline) '(:inline :baseline)))
               (setf (symbol-function slot-scan)
                     (if (eq mode :baseline) #'pipeline-original-slot-scan original-slot-scan))
               (setf (symbol-function helper)
                     (if (eq mode :baseline) #'allocate-struct-pipeline-baseline original-helper)
                     (symbol-function 'gc-benchmark) (cdr (assoc mode variants)))
               (format t "~&PAIR ~D ~S~%" pair mode)
               (measure-structure-allocation depth 1))))
      (setf (symbol-function helper) original-helper
            (symbol-function generator) original-generator
            (symbol-function slot-scan) original-slot-scan)
      (proclaim `(inline ,helper))
      (if original-benchmark
          (setf (symbol-function 'gc-benchmark) original-benchmark)
          (fmakunbound 'gc-benchmark)))))
