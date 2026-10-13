;;;; Load in an x86-64 Mezzano guest, then call (RUN-X86-R15-REGRESSIONS).
;;;; Each probe allocates a cons held only in R15 while the supplied thunk runs.
(in-package :cl-user)

(mezzano.internals::define-lap-function x86-r15-probe ((thunk))
  (:gc :no-frame :layout #*0)
  (mezzano.lap.x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (mezzano.lap.x86:mov64 :rbp :rsp)
  (:gc :frame)
  (mezzano.lap.x86:sub64 :rsp 16)
  (mezzano.lap.x86:mov64 (:stack 0) :r15)
  (:gc :frame :layout #*1)
  (mezzano.lap.x86:mov64 (:stack 1) :r8)
  (:gc :frame :layout #*11)
  (mezzano.lap.x86:mov64 :r8 (:constant :r15-regression))
  (mezzano.lap.x86:mov64 :r9 #.(ash 12345 mezzano.internals::+n-fixnum-bits+))
  (mezzano.lap.x86:mov64 :rcx #.(ash 2 mezzano.internals::+n-fixnum-bits+))
  (mezzano.lap.x86:call (:named-call cons))
  (mezzano.lap.x86:mov64 :r15 :r8)
  ;; Eliminate all other register copies of the newly allocated cons.
  (mezzano.lap.x86:mov64 :r8 :r14)
  (mezzano.lap.x86:mov64 :r9 :r14)
  (mezzano.lap.x86:mov64 :r10 :r14)
  (mezzano.lap.x86:mov64 :r11 :r14)
  (mezzano.lap.x86:mov64 :r12 :r14)
  (mezzano.lap.x86:mov64 :r13 :r14)
  (mezzano.lap.x86:mov64 :rbx (:stack 1))
  (mezzano.lap.x86:xor32 :ecx :ecx)
  (mezzano.lap.x86:call (:object :rbx #.mezzano.internals::+function-entry-point+))
  (mezzano.lap.x86:mov64 :r8 :r15)
  (mezzano.lap.x86:mov64 :r15 (:stack 0))
  (mezzano.lap.x86:mov64 :rcx #.(ash 1 mezzano.internals::+n-fixnum-bits+))
  (mezzano.lap.x86:leave)
  (:gc :no-frame :layout #*0)
  (mezzano.lap.x86:ret))

(declaim (notinline x86-r15-leaf x86-r15-tail x86-r15-tail-many
                    x86-r15-optionals x86-r15-rest))
(defun x86-r15-leaf (a b c d e f g h)
  (list a b c d e f g h))
(defun x86-r15-tail (a b c d)
  (x86-r15-leaf a b c d a b c d))
(defun x86-r15-tail-many (function a b c d e f g h)
  (funcall function a b c d e f g h))

(defun x86-r15-optionals (a b c d e &optional f g)
  (list a b c d e f g))
(defun x86-r15-rest (a b c d e &rest rest)
  (list a b c d e rest))
(declaim (notinline x86-r15-seven-values))
(defun x86-r15-seven-values ()
  (values (cons :a 1) (cons :b 2) 3 4 5 6 7))

(defun x86-r15-stress-calls ()
  (let ((a (cons :a 1)) (b (cons :b 2)) (c (cons :c 3)) (d (cons :d 4))
        (e (cons :e 5)) (f (cons :f 6)) (g (cons :g 7)) (h (cons :h 8)))
    (assert (equal (x86-r15-leaf a b c d e f g h) (list a b c d e f g h)))
    (assert (equal (x86-r15-tail a b c d) (list a b c d a b c d)))
    (assert (equal (x86-r15-tail-many #'x86-r15-leaf a b c d e f g h)
                   (list a b c d e f g h)))
    (assert (equal (x86-r15-optionals a b c d e) (list a b c d e nil nil)))
    (assert (equal (x86-r15-optionals a b c d e f g) (list a b c d e f g)))
    (assert (equal (x86-r15-rest a b c d e f g h) (list a b c d e (list f g h))))
    (assert (= (+ (cdr a) (cdr b) (cdr c) (cdr d)
                  (cdr e) (cdr f) (cdr g) (cdr h)) 36))))

(defun run-x86-r15-regressions ()
  (flet ((probe (name thunk)
           (mezzano.supervisor:debug-print-line "R15 BEGIN " name)
           (let ((result (x86-r15-probe thunk)))
             (assert (and (consp result) (eq (car result) :r15-regression)
                          (eql (cdr result) 12345))))
           (mezzano.supervisor:debug-print-line "R15 PASS " name)))
    (probe :calls #'x86-r15-stress-calls)
    (probe :multiple-values
           (lambda ()
             (multiple-value-bind (a b c d e f g)
                 (x86-r15-seven-values)
               (assert (= (+ (cdr a) (cdr b) c d e f g) 28)))))
    (probe :non-local-exit
           (lambda ()
             (assert (equal
                      (block done
                        (unwind-protect
                             (funcall (lambda () (return-from done (list :exit 42))))
                          (x86-r15-stress-calls)))
                      '(:exit 42)))))
    (probe :runtime-catch-throw
           (lambda ()
             (let ((tag (list :r15-catch)))
               (assert
                (equal (multiple-value-list
                        (mezzano.internals::%catch
                         tag
                         (lambda ()
                           (unwind-protect
                                (mezzano.internals::%throw tag (list :caught 42 3 4 5 6 7))
                             (x86-r15-stress-calls)))))
                       '(:caught 42 3 4 5 6 7))))))
    (probe :warning-restarts
           (lambda ()
             (let ((inner 0) (outer 0))
               (handler-bind ((warning (lambda (condition)
                                         (incf outer)
                                         (muffle-warning condition))))
                 (handler-bind ((warning (lambda (condition)
                                           (declare (ignore condition))
                                           (incf inner)
                                           (x86-r15-stress-calls))))
                   (warn "R15 warning restart regression ~D" 42)))
               (assert (= inner outer 1)))))
    (probe :voluntary-switch
           (lambda ()
             (dotimes (i 10)
               (mezzano.supervisor:thread-yield)
               (sleep 0.01)
               (x86-r15-stress-calls))))
    (probe :partial-conversion
           (lambda ()
             (let ((thread
                     (mezzano.supervisor:make-thread
                      (lambda ()
                        (x86-r15-probe #'mezzano.supervisor::stop-current-thread))
                      :name "R15 stopped-thread regression")))
               (loop until (eq (mezzano.supervisor::sample-thread-state thread) :stopped)
                     do (mezzano.supervisor:thread-yield))
               (assert (not (mezzano.supervisor:thread-full-save-p thread)))
               (mezzano.internals::gc :full t)
               (mezzano.supervisor::convert-thread-to-full-save thread)
               (assert (mezzano.supervisor:thread-full-save-p thread))
               ;; Materializing an interrupt frame requires the same exclusion
               ;; as foothold injection: current caller owns the world stop.
               (mezzano.supervisor::with-world-stopped
                 (mezzano.supervisor::convert-thread-to-partial-save thread)
                 (mezzano.supervisor::convert-thread-to-full-save thread))
               (mezzano.supervisor::resume-thread thread)
               (let ((result (first (mezzano.supervisor:thread-join thread))))
                 (assert (and (consp result) (eq (car result) :r15-regression)
                              (eql (cdr result) 12345)))))))
    (probe :foothold
           (lambda ()
             (let ((ran nil)
                   (thread
                     (mezzano.supervisor:make-thread
                      (lambda ()
                        (x86-r15-probe #'mezzano.supervisor::stop-current-thread))
                      :name "R15 foothold regression")))
               (loop until (eq (mezzano.supervisor::sample-thread-state thread) :stopped)
                     do (mezzano.supervisor:thread-yield))
               (assert (not (mezzano.supervisor:thread-full-save-p thread)))
               (mezzano.supervisor::establish-thread-foothold
                thread (lambda () (setf ran t)))
               (assert (mezzano.supervisor:thread-full-save-p thread))
               (mezzano.supervisor::resume-thread thread)
               (let ((result (first (mezzano.supervisor:thread-join thread))))
                 (assert ran)
                 (assert (and (consp result) (eq (car result) :r15-regression)
                              (eql (cdr result) 12345)))))))
    (probe :gc
           (lambda ()
             (mezzano.internals::gc)
             (x86-r15-stress-calls)
             (mezzano.internals::gc :full t)))
    (probe :continuation
           (lambda ()
             (let ((tag (mezzano.delimited-continuations:make-prompt-tag))
                   (continuation nil))
               (assert
                (= (mezzano.delimited-continuations:call-with-prompt
                    tag
                    (lambda ()
                      (+ 10 (mezzano.delimited-continuations:abort-to-prompt tag 7)))
                    (lambda (k value) (setf continuation k) value))
                   7))
               ;; The suspended continuation must survive a moving collection.
               (mezzano.internals::gc :full t)
               (assert (= (funcall continuation 32) 42)))))
    (mezzano.supervisor:debug-print-line "R15 ALL PASS")
    t))
