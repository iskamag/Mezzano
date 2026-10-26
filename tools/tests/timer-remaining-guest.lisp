;;;; Load in a running Mezzano image.
(let* ((timer (mezzano.supervisor:make-timer :name 'remaining-time-regression))
       (now (get-internal-run-time))
       (units internal-time-units-per-second))
  (unwind-protect
       (progn
         (assert (null (mezzano.supervisor:timer-remaining timer)))
         (mezzano.supervisor:timer-arm-absolute (+ now (* 60 units)) timer)
         (assert (plusp (mezzano.supervisor:timer-remaining timer)))
         (assert (plusp (mezzano.supervisor:timer-disarm timer)))
         (assert (null (mezzano.supervisor:timer-disarm timer)))
         (mezzano.supervisor:timer-arm-absolute (- (get-internal-run-time) units) timer)
         (assert (zerop (mezzano.supervisor:timer-remaining timer)))
         (assert (zerop (mezzano.supervisor:timer-disarm timer)))
         (format t "Timer remaining regressions: 6 checks passed.~%"))
    (mezzano.supervisor:timer-disarm timer)))
