;;;; Invoke (mezzano.pr220-tlb-test:run) in a booted SMP guest.
;;;; The workers serialize through the real VM lock and force repeated
;;;; all-peer TLB rendezvous while other CPUs enter and leave idle.

(defpackage :mezzano.pr220-tlb-test
  (:use :cl)
  (:export #:run))

(in-package :mezzano.pr220-tlb-test)

(defun shootdown-worker (iterations)
  (dotimes (i iterations)
    (mezzano.supervisor::with-rw-lock-write
        (mezzano.supervisor::*vm-lock*)
      (mezzano.supervisor::begin-tlb-shootdown)
      (mezzano.supervisor::tlb-shootdown-all)
      (mezzano.supervisor::finish-tlb-shootdown))
    (when (zerop (mod i 8))
      (mezzano.supervisor::broadcast-wakeup-ipi)))
  t)

(defun run (&key (workers 4) (iterations 200))
  (check-type workers (integer 1 32))
  (check-type iterations (integer 1 100000))
  (let ((threads
          (loop repeat workers
                collect (mezzano.supervisor::make-thread
                         (lambda () (shootdown-worker iterations))
                         :name "PR220 TLB stress"))))
    (mezzano.supervisor::broadcast-wakeup-ipi)
    (dolist (thread threads)
      (assert (mezzano.supervisor::thread-join thread)))
    (format t "PR220 TLB stress: ~D workers x ~D shootdowns passed.~%"
            workers iterations)
    t))
