;;;; Load in an x86-64 guest, then (R15-BENCHMARK:RUN).
;;;; Compares allocator off/on in the same callee-save ABI and live image.
;;;; The complete owned graph is recompiled in both modes for every pair.
;;;; Timing excludes compilation, explicit pre-trial GC, and warmup.
(defpackage :r15-benchmark (:use :cl) (:export :run :graph-forms))
(in-package :r15-benchmark)

(defvar *capture-lap* nil)
(defvar *compile-stats* nil)
(defvar *sink* nil)

;; The strict control helper is shared unchanged by both graphs. Its state
;; uses raw integer registers, so it never allocates or touches R15.
(mezzano.internals::define-lap-function raw-control ((repeats input))
  (:gc :no-frame :layout #*0)
  (mezzano.lap.x86:mov64 :rax :r8)
  (mezzano.lap.x86:sar64 :rax #.mezzano.internals::+n-fixnum-bits+)
  (mezzano.lap.x86:shl64 :rax 6)
  (mezzano.lap.x86:xor32 :edx :edx)
  (mezzano.lap.x86:test64 :rax :rax)
  (mezzano.lap.x86:jz done)
  again
  (mezzano.lap.x86:mov64 :rcx :rax)
  (mezzano.lap.x86:and64 :rcx 63)
  (mezzano.lap.x86:imul64 :rcx 17)
  (mezzano.lap.x86:add64 :rdx :rcx)
  (mezzano.lap.x86:add64 :rdx 31)
  (mezzano.lap.x86:and64 :rdx #xffff)
  (mezzano.lap.x86:sub64 :rax 1)
  (mezzano.lap.x86:jnz again)
  done
  (mezzano.lap.x86:shl64 :rdx #.mezzano.internals::+n-fixnum-bits+)
  (mezzano.lap.x86:mov64 :r8 :rdx)
  (mezzano.lap.x86:mov64 :rcx #.(ash 1 mezzano.internals::+n-fixnum-bits+))
  (mezzano.lap.x86:ret))

(defun contains-r15-p (object)
  (if (consp object)
      (or (contains-r15-p (car object)) (contains-r15-p (cdr object)))
      (eq object :r15)))

(defmethod mezzano.compiler.backend:perform-target-lap-generation :around
    (function debug-map spills layout (target mezzano.compiler:x86-64-target))
  (multiple-value-bind (lap environment) (call-next-method)
    (when *capture-lap*
      (let* ((debug (find :debug lap :key (lambda (item) (and (consp item) (car item)))))
             (saved-slot (second (assoc :r15 (second debug))))
             (save-ops 0) (body-ops 0) (stack-ops 0))
        (dolist (item lap)
          (when (and (consp item) (symbolp (car item))
                     (eq (symbol-package (car item)) (find-package :mezzano.lap.x86)))
            (when (find-if (lambda (operand) (and (consp operand) (eq (car operand) :stack)))
                           (cdr item))
              (incf stack-ops))
            (when (contains-r15-p (cdr item))
              (if (and saved-slot (eq (car item) 'mezzano.lap.x86:mov64)
                       (or (equal (cdr item) (list (list :stack saved-slot) :r15))
                           (equal (cdr item) (list :r15 (list :stack saved-slot)))))
                  (incf save-ops)
                  (incf body-ops)))))
        (push (list :spill-vregs (hash-table-count spills) :layout-slots (length layout)
                    :stack-operand-ops stack-ops :r15-save-restore-ops save-ops
                    :r15-body-ops body-ops)
              *compile-stats*)))
    (values lap environment)))

(defun graph-name (mode name)
  (intern (format nil "~A-~A" (if mode "ON" "OFF") name) :r15-benchmark))

(defun graph-forms (mode)
  "Named lambda forms for the complete owned call graph, leaf-first."
  (let ((mix (graph-name mode 'mix)) (score (graph-name mode 'score))
        (walk (graph-name mode 'walk)) (transform (graph-name mode 'transform))
        (batch (graph-name mode 'batch)) (reduce (graph-name mode 'reduce))
        (calls (graph-name mode 'calls)) (tree (graph-name mode 'tree))
        (records (graph-name mode 'records)) (arithmetic (graph-name mode 'arithmetic))
        (control (graph-name mode 'control)))
    `((,mix . (lambda (x seed)
                (declare (optimize (speed 3) (safety 0) (debug 0))
                         (type fixnum x seed))
                (logand #xffff (+ x (logxor seed #x5a5a)))))
      ;; NODE must survive an ordinary call to MIX. No local DO/tagbody:
      ;; conservative NLX marking would otherwise force caller-save spills.
      (,score . (lambda (node seed)
                  (declare (optimize (speed 3) (safety 1) (debug 0)) (type fixnum seed))
                  (let* ((a (,mix (car node) seed))
                         (b (,mix (cdr node) a))
                         (c (,mix (car node) b))
                         (d (,mix (cdr node) c)))
                    (logand #xffff (+ d (car node) (cdr node))))))
      (,walk . (lambda (node)
                 (declare (optimize (speed 3) (safety 1) (debug 0)))
                 (if node
                     (let ((left (,walk (cadr node))))
                       (logand #xffff (+ (car node) left (,walk (cddr node)))))
                     0)))
      (,transform . (lambda (node seed)
                      (declare (optimize (speed 3) (safety 1) (debug 0)))
                      (let ((first (,score node seed)))
                        (cons first (,score node (logand #xffff (+ seed 7)))))))
      (,batch . (lambda (input seed)
                  (declare (optimize (speed 3) (safety 1) (debug 0))
                           (type simple-vector input))
                  (let ((output nil))
                    (dotimes (i 64 output)
                      (push (,transform (svref input i) (logand #xffff (+ seed i))) output)))))
      (,reduce . (lambda (records)
                   (declare (optimize (speed 3) (safety 1) (debug 0)))
                   (let ((sum 0))
                     (dolist (record records sum)
                       (setf sum (logand #xffff (+ sum (car record) (cdr record))))))))
      (,calls . (lambda (repeats input)
                  (declare (optimize (speed 3) (safety 1) (debug 0))
                           (type fixnum repeats) (type simple-vector input))
                  (let ((sum 0))
                    (dotimes (r repeats sum)
                      (dotimes (i 64)
                        (setf sum (logand #xffff
                                         (+ sum (,score (svref input i) i)))))))))
      (,tree . (lambda (repeats input)
                 (declare (optimize (speed 3) (safety 1) (debug 0)) (type fixnum repeats))
                 (let ((sum 0))
                   (dotimes (r repeats sum)
                     (setf sum (logand #xffff (+ sum (,walk input))))))))
      (,records . (lambda (repeats input)
                    (declare (optimize (speed 3) (safety 1) (debug 0)) (type fixnum repeats))
                    (let ((sum 0))
                      (dotimes (r repeats sum)
                        (setf sum (logand #xffff (+ sum (,reduce (,batch input 19)))))))))
      ;; Call-free arithmetic still benefits from general register pressure;
      ;; report it separately from the strict unchanged-code control.
      (,arithmetic . (lambda (repeats input)
                    (declare (ignore input) (optimize (speed 3) (safety 1) (debug 0))
                             (type fixnum repeats))
                    (let ((sum 0))
                      (declare (type fixnum sum))
                      (dotimes (r repeats sum)
                        (dotimes (i 64)
                          (setf sum (logand #xffff (+ sum (* 17 i) 31))))))))
      ;; These wrappers provide compilation statistics, while timed CONTROL
      ;; trials call the same RAW-CONTROL function object in both modes.
      (,control . (lambda (repeats input)
                    (declare (optimize (speed 3) (safety 0) (debug 0)))
                    (raw-control repeats input))))))

(defun emit-record (record)
  (let ((*print-pretty* nil) (*print-length* nil) (*print-level* nil))
    (format t "~&R15-BENCH ~S~%" record))
  (finish-output))

(defun compile-graph (mode pair)
  (let* ((forms (graph-forms mode))
         (mezzano.compiler::*use-x86-r15* mode)
         (mezzano.compiler::*perform-tce* nil)
         (*capture-lap* nil) (*compile-stats* nil)
         (bytes 0))
    (eval `(declaim (notinline ,@(mapcar #'car forms))))
    (dolist (form forms)
      (let ((previous *compile-stats*))
        (let ((*capture-lap* t))
          (compile (car form) (cdr form)))
        ;; FUNCTION-CODE-SIZE includes the 16-byte object/entry header.
        ;; Retain code alignment padding in this static byte metric.
        (let ((code-bytes (- (mezzano.internals::function-code-size (fdefinition (car form))) 16))
              (stats (ldiff *compile-stats* previous)))
          (incf bytes code-bytes)
          (when (eq (car form) (graph-name mode 'control))
            (assert (zerop (loop for stat in stats sum (getf stat :r15-body-ops)))))
          (emit-record (list :function-compile :pair pair :mode (if mode :on :off)
                             :name (car form) :machine-bytes code-bytes :lap-stats stats)))))
    (let ((totals (list :compile :pair pair :mode (if mode :on :off)
                        :functions (length forms) :generated-functions (length *compile-stats*)
                        :machine-bytes bytes)))
      (dolist (key '(:spill-vregs :layout-slots :stack-operand-ops
                    :r15-save-restore-ops :r15-body-ops))
        (setf totals (append totals (list key (loop for stat in *compile-stats*
                                                  sum (getf stat key))))))
      ;; Disabled graphs may save incoming R15 for NLX, but never allocate
      ;; it to body values. Enabled graphs must actually exercise allocation.
      (assert (if mode (plusp (getf (cdr totals) :r15-body-ops))
                  (zerop (getf (cdr totals) :r15-body-ops))))
      (emit-record totals))))

(defun make-tree (depth seed)
  (when (plusp depth)
    (cons (logand seed #xff)
          (cons (make-tree (1- depth) (+ (* seed 2) 1))
                (make-tree (1- depth) (+ (* seed 2) 2))))))

(defun reference-checksum (name repeats input)
  ;; Compute one unit independently of the recompiled graph, then scale.
  (labels ((score (node seed)
             (dolist (value (list (car node) (cdr node) (car node) (cdr node)))
               (setf seed (logand #xffff (+ value (logxor seed #x5a5a)))))
             (+ seed (car node) (cdr node)))
           (tree (node)
             (if node (+ (car node) (tree (cadr node)) (tree (cddr node))) 0)))
    (logand #xffff
            (* repeats
               (ecase name
                 (calls (loop for i below 64 sum (score (svref input i) i)))
                 (tree (tree input))
                 (records (loop for i below 64
                                sum (+ (logand #xffff (score (svref input i) (+ 19 i)))
                                       (logand #xffff (score (svref input i) (+ 26 i))))))
                 ((arithmetic control) (+ (* 17 (/ (* 64 63) 2)) (* 64 31))))))))

(defun benchmark-function (mode name)
  ;; Separately compiled wrappers have different PC-relative relocation
  ;; bytes. Use one identical function object for the strict timing control.
  (fdefinition (if (eq name 'control) 'raw-control (graph-name mode name))))

(defun time-trial (function repeats input)
  ;; Snapshot counters without allocating a result structure before timing.
  (let* ((self (mezzano.supervisor:current-thread))
         (bytes (mezzano.supervisor:thread-bytes-consed self))
         (gc-cycles mezzano.internals::*gc-cycles*)
         (gc-major mezzano.internals::*gc-major-cycles*)
         (gc-minor mezzano.internals::*gc-minor-cycles*)
         (gc-time mezzano.internals::*gc-time*)
         (run-time (mezzano.supervisor:thread-run-time self))
         (allocation-time (mezzano.supervisor:thread-allocation-time self))
         (wall (get-internal-real-time))
         (start (mezzano.supervisor:get-high-precision-timer))
         (checksum (funcall function repeats input))
         (ticks (- (mezzano.supervisor:get-high-precision-timer) start))
         (end-wall (get-internal-real-time))
         (end-bytes (mezzano.supervisor:thread-bytes-consed self))
         (end-run-time (mezzano.supervisor:thread-run-time self))
         (end-allocation-time (mezzano.supervisor:thread-allocation-time self))
         (end-gc-cycles mezzano.internals::*gc-cycles*)
         (end-gc-major mezzano.internals::*gc-major-cycles*)
         (end-gc-minor mezzano.internals::*gc-minor-cycles*)
         (end-gc-time mezzano.internals::*gc-time*))
    (assert (plusp ticks))
    (list :tsc-ticks ticks
          :seconds (/ (float ticks 1d0) mezzano.supervisor::*cpu-speed*)
          :wall-ticks (- end-wall wall)
          :thread-run-ticks (- end-run-time run-time)
          :allocation-ticks (- end-allocation-time allocation-time)
          :bytes (- end-bytes bytes)
          :gc-cycles (- end-gc-cycles gc-cycles)
          :gc-major (- end-gc-major gc-major)
          :gc-minor (- end-gc-minor gc-minor)
          :gc-seconds (- end-gc-time gc-time)
          :checksum checksum)))

(defun calibrate (name input target)
  ;; Common repeat count chosen from the slower mode; never time different
  ;; amounts of work in an off/on pair. Calibration trials are not samples.
  (let ((repeats 1))
    (loop
      (let* ((off (time-trial (benchmark-function nil name) repeats input))
             (on (time-trial (benchmark-function t name) repeats input))
             (seconds (max (getf off :seconds) (getf on :seconds))))
        (assert (= (getf off :checksum) (getf on :checksum)))
        (when (>= seconds (/ target 3))
          (return (max 1 (round (* repeats (/ target seconds))))))
        (setf repeats (* repeats 4))))))

(defun median (values)
  (let* ((sorted (sort (copy-list values) #'<)) (n (length sorted)))
    (if (oddp n) (nth (floor n 2) sorted)
        (/ (+ (nth (1- (/ n 2)) sorted) (nth (/ n 2) sorted)) 2))))

(defun run (&key (pairs 11) (target-seconds 0.2d0))
  "Print raw paired samples and summaries. OFF/ON are allocator modes.
The kernel, standard library, ABI, input, and runner remain the same.
GC counters/time are image-wide; allocation bytes are per benchmark thread.
TSC ticks measure elapsed reference time, not core execution cycles."
  (assert (and (>= pairs 3) (plusp target-seconds)))
  (emit-record (list :configuration :pairs pairs :target-seconds target-seconds
                     :llf-version mezzano.internals::*llf-version*
                     :tsc-hz mezzano.supervisor::*cpu-speed*
                     :internal-hz internal-time-units-per-second :tce nil
                     :comparison :allocator-in-same-callee-save-abi))
  (let* ((records (make-array 64))
         (tree (make-tree 8 1))
         (workloads nil) (samples nil))
    (dotimes (i 64) (setf (svref records i) (cons (+ i 1) (+ (* i 3) 7))))
    (compile-graph nil :calibration)
    (compile-graph t :calibration)
    (assert (eq (benchmark-function nil 'control) (benchmark-function t 'control)))
    (dolist (entry (list (list 'calls records) (list 'tree tree)
                        (list 'records records) (list 'arithmetic nil) (list 'control nil)))
      (destructuring-bind (name input) entry
        (let* ((repeats (calibrate name input target-seconds))
               (off (funcall (benchmark-function nil name) repeats input))
               (on (funcall (benchmark-function t name) repeats input)))
          (assert (= off on (reference-checksum name repeats input)))
          (push (list name input repeats off) workloads)
          (emit-record (list :calibration :workload name :repeats repeats :checksum off)))))
    (setf workloads (nreverse workloads))
    (dotimes (index pairs)
      (let* ((pair (1+ index)) (order (if (evenp index) '(nil t) '(t nil))))
        ;; Recompile the entire owned graph each pair, alternating order.
        (dolist (mode order) (compile-graph mode pair))
        (assert (eq (benchmark-function nil 'control) (benchmark-function t 'control)))
        (dolist (workload workloads)
          (destructuring-bind (name input repeats expected) workload
            (dolist (mode order)
              (let ((function (benchmark-function mode name)))
                (mezzano.internals::gc :full t)
                (setf *sink* (funcall function (max 1 (floor repeats 10)) input))
                (let ((metrics (time-trial function repeats input)))
                  (assert (= expected (getf metrics :checksum)))
                  (setf *sink* (getf metrics :checksum))
                  (push (list name pair mode metrics) samples)
                  (emit-record (append (list :trial :workload name :pair pair
                                             :order (if (evenp index) :off-on :on-off)
                                             :mode (if mode :on :off) :repeats repeats)
                                       metrics)))))))))
    (dolist (workload workloads)
      (let ((name (first workload)) (ratios nil) (off-times nil) (on-times nil))
        (dotimes (index pairs)
          (let* ((pair (1+ index))
                 (off (fourth (find-if (lambda (sample)
                                        (and (eq (first sample) name) (= (second sample) pair)
                                             (not (third sample)))) samples)))
                 (on (fourth (find-if (lambda (sample)
                                       (and (eq (first sample) name) (= (second sample) pair)
                                            (third sample))) samples))))
            (push (getf off :seconds) off-times)
            (push (getf on :seconds) on-times)
            (push (/ (getf off :seconds) (getf on :seconds)) ratios)))
        (emit-record (list :summary :workload name :pairs pairs
                           :off-seconds-median (median off-times) :on-seconds-median (median on-times)
                           :paired-off-over-on-median (median ratios)
                           :paired-off-over-on-min (apply #'min ratios)
                           :paired-off-over-on-max (apply #'max ratios)))))
    (emit-record (list :complete :pairs pairs :workloads (length workloads) :sink *sink*))
    t))
