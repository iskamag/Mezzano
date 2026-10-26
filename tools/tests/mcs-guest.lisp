;;; Guest stress entry: (mezzano.tools.mcs-guest:run)
;;; Load into a rebuilt image; this file deliberately does not run on load.

(defpackage :mezzano.tools.mcs-guest
  (:use :cl)
  (:export #:run))
(in-package :mezzano.tools.mcs-guest)

(mezzano.internals::defglobal *iterations* 0)
(mezzano.internals::defglobal *counter* 0)
(mezzano.internals::defglobal *footholds* 0)

(defun foothold-callback ()
  (mezzano.internals::%atomic-fixnum-add-symbol '*footholds* 1))

(defun count-one ()
  (mezzano.supervisor::safe-without-interrupts ()
    (mezzano.supervisor::with-global-thread-lock ()
      (incf *counter*))))

(defun normal-worker (gate)
  (when gate
    (mezzano.supervisor::event-wait gate))
  (dotimes (i *iterations*)
    (count-one)
    (when (zerop (mod i 4))
      (mezzano.supervisor::thread-yield)))
  ;; The queued self foothold exercises the lock path used while footholds
  ;; are inhibited, followed by normal delivery when the scope exits.
  (mezzano.supervisor::without-footholds
    (mezzano.supervisor::establish-thread-foothold
     (mezzano.supervisor::current-thread) #'foothold-callback)))

(defun ephemeral-worker ()
  (dotimes (i *iterations*)
    (count-one)
    ;; A wired ephemeral thread with IRQs already off stays on its own stack
    ;; in %CALL-ON-WIRED-STACK-WITHOUT-INTERRUPTS. The MCS switch trampoline
    ;; must leave that stack before handing the lock to another CPU.
    (mezzano.supervisor::%disable-interrupts)
    (mezzano.supervisor::thread-yield))
  (mezzano.supervisor::thread-final-cleanup nil))

(defun run (&key (iterations 80) (normal-workers 4) (ephemeral-workers 3))
  "Stress MCS handoff, sleeping/waking, footholds and wired-stack yields on SMP."
  (unless (> mezzano.supervisor::*n-up-cpus* 1)
    (error "MCS guest stress requires at least two online CPUs"))
  (setf *iterations* iterations
        *counter* 0
        *footholds* 0)
  (let* ((gate (mezzano.supervisor::make-event :name 'mcs-stress-gate))
         (normal (loop for i below normal-workers
                       collect (let ((wait-on-gate (evenp i)))
                                 (mezzano.supervisor::make-thread
                                  (lambda () (normal-worker (when wait-on-gate gate)))
                                  :name (list 'mcs-normal i)))))
         (ephemeral (loop repeat ephemeral-workers
                          collect (mezzano.supervisor::make-ephemeral-thread
                                   #'ephemeral-worker :sleeping
                                   :name (mezzano.runtime::copy-string-in-area
                                          "MCS ephemeral worker" :wired)))))
    ;; Ephemeral entry functions need an event because they call the normal
    ;; final-cleanup path rather than returning through a thread trampoline.
    (dolist (thread ephemeral)
      (setf (mezzano.supervisor::thread-join-event thread)
            (mezzano.supervisor::make-event :name thread)))
    (dolist (thread ephemeral)
      (mezzano.supervisor::safe-without-interrupts (thread)
        (mezzano.supervisor::wake-thread thread)))
    (setf (mezzano.supervisor::event-state gate) t)
    (dolist (thread normal)
      (mezzano.supervisor::thread-join thread))
    (dolist (thread ephemeral)
      (mezzano.supervisor::thread-join thread))
    (let ((expected (* iterations (+ normal-workers ephemeral-workers))))
      (unless (= *counter* expected)
        (error "MCS count ~D; expected ~D" *counter* expected))
      (unless (= *footholds* normal-workers)
        (error "MCS footholds ~D; expected ~D" *footholds* normal-workers))
      (format t "MCS guest stress passed: ~D increments, ~D footholds, ~D CPUs~%"
              *counter* *footholds* mezzano.supervisor::*n-up-cpus*)
      t)))
