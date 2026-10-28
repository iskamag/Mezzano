;;;; Original shared cons allocator from 68b7e85c, renamed for live A/B trials.
;;;; Only function names and their calls differ; runtime metering is unchanged.
(in-package :mezzano.runtime)

(sys.int::define-lap-function benchmark-shared-do-cons ((car cdr))
  (:gc :no-frame :layout #*0)
  ;; Attempt to quickly allocate a benchmark-shared-cons.
  ;; Returns (values car cdr t) on failure, just the benchmark-shared-cons on success.
  ;; R8 = car; R9 = cdr
  ;; Fetch symbol value cells.
  (sys.lap-x86:mov64 :r13 (:symbol-global-cell sys.int::*cons-area-young-gen-bump*))
  (sys.lap-x86:mov64 :r12 (:symbol-global-cell sys.int::*young-gen-newspace-bit-raw*))
  (sys.lap-x86:mov64 :r11 (:symbol-global-cell sys.int::*cons-area-young-gen-limit*))
  ;; R13 = bump. R12 = newspace-bit. R11 = limit.
  (:gc :no-frame :layout #*0 :restart t)
  ;; Fetch and increment the current bump pointer.
  (sys.lap-x86:mov64 :rbx #.(ash 16 #.sys.int::+n-fixnum-bits+)) ; 16, size of benchmark-shared-cons
  (sys.lap-x86:lock)
  (sys.lap-x86:xadd64 (:object :r13 #.sys.int::+symbol-value-cell-value+) :rbx)
  ;; RBX is old bump pointer, the address of the benchmark-shared-cons.
  ;; Find the new bump pointer.
  (sys.lap-x86:lea64 :rsi (:rbx #.(ash 16 #.sys.int::+n-fixnum-bits+)))
  ;; Test against limit.
  (sys.lap-x86:cmp64 :rsi (:object :r11 #.sys.int::+symbol-value-cell-value+))
  (sys.lap-x86:ja SLOW-PATH)
  ;; Generate the benchmark-shared-cons object.
  ;; Unfixnumize address. This still looks like a fixnum due to alignment.
  (sys.lap-x86:shr64 :rbx #.sys.int::+n-fixnum-bits+)
  ;; Set address bits, tag bits, and the mark bit.
  (sys.lap-x86:mov64 :rax #.(logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                                    sys.int::+tag-cons+))
  (sys.lap-x86:or64 :rax (:object :r12 #.sys.int::+symbol-value-cell-value+))
  (sys.lap-x86:or64 :rbx :rax)
  ;; RBX now holds a valid benchmark-shared-cons, with the CAR and CDR set to zero.
  ;; It is safe to leave the restart region.
  (:gc :no-frame :layout #*0)
  ;; Initialize the CAR & CDR outside the restart region to minimise the potential restarts.
  (sys.lap-x86:mov64 (:car :rbx) :r8)
  (sys.lap-x86:mov64 (:cdr :rbx) :r9)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :r8 :rbx)
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  (sys.lap-x86:mov64 :r10 t)
  (sys.lap-x86:mov32 :ecx #.(ash 3 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function benchmark-shared-cons ((car cdr))
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  ;; Attempt to quickly allocate a benchmark-shared-cons. Will call benchmark-shared-slow-cons if things get too hairy.
  ;; R8 = car; R9 = cdr
  ;; Check argument count.
  (sys.lap-x86:cmp64 :rcx #.(ash 2 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH-BAD-ARGS)
  (:gc :no-frame :layout #*0)
  ;; Update allocation meter.
  (sys.lap-x86:fs)
  (sys.lap-x86:add64 (:object-location nil #.mezzano.supervisor::+x86-64-cpu-cons-allocation-count+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:fs)
  (sys.lap-x86:add64 (:object-location nil #.mezzano.supervisor::+x86-64-cpu-bytes-consed+)
                     #.(ash 16 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:gs)
  (sys.lap-x86:add64 (:object nil #.mezzano.supervisor::+thread-bytes-consed+)
                     #.(ash 16 sys.int::+n-fixnum-bits+))
  ;; Check *ENABLE-ALLOCATION-PROFILING*
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell *enable-allocation-profiling*))
  (sys.lap-x86:cmp64 (:object :rbx #.sys.int::+symbol-value-cell-value+) nil)
  (sys.lap-x86:jne SLOW-PATH)
  ;; Check *GC-IN-PROGRESS*.
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell sys.int::*gc-in-progress*))
  (sys.lap-x86:cmp64 (:object :rbx #.sys.int::+symbol-value-cell-value+) nil)
  (sys.lap-x86:jne SLOW-PATH)
  ;; Try the real fast allocator.
  (sys.lap-x86:call (:named-call benchmark-shared-do-cons))
  (sys.lap-x86:cmp64 :rcx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH)
  ;; Done. Return everything.
  (sys.lap-x86:fs)
  (sys.lap-x86:add64 (:object-location nil #.mezzano.supervisor::+x86-64-cpu-cons-fast-path-hits+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  ;; Tail call into benchmark-shared-slow-cons.
  (sys.lap-x86:mov32 :ecx #.(ash 2 #.sys.int::+n-fixnum-bits+))
  SLOW-PATH-BAD-ARGS
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:named-call benchmark-shared-slow-cons)))

(defun benchmark-shared-slow-cons (car cdr)
  (when sys.int::*gc-in-progress*
    (mezzano.supervisor:panic "Allocating during GC!"))
  (log-allocation-profile-entry 2)
  (let ((gc-count 0)
        (start-time (mezzano.supervisor:get-high-precision-timer)))
    (tagbody
     OUTER-LOOP
       (mezzano.supervisor:without-footholds
         (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
           (mezzano.supervisor:with-mutex (*allocator-lock*)
             (mezzano.supervisor:with-pseudo-atomic
               (tagbody
                INNER-LOOP
                  ;; Call the real allocator.
                  (multiple-value-bind (result blah failurep)
                      (benchmark-shared-do-cons car cdr)
                    (declare (ignore blah))
                    (when (not failurep)
                      (update-allocation-time start-time)
                      (return-from benchmark-shared-slow-cons result)))
                  ;; No memory. If there's memory available, then expand the area, otherwise run the GC.
                  ;; Running the GC cannot be done when pseudo-atomic.
                  (cond ((expand-allocation-area :cons
                                                 16
                                                 '*cons-area-expansion-granularity*
                                                 'sys.int::*cons-area-young-gen-limit*
                                                 sys.int::+address-tag-cons+)
                         ;; Successfully expanded the area Retry the allocation.
                         (go INNER-LOOP))
                        (t
                         ;; No memory do expand, bail out and run the GC.
                         ;; This cannot be done when pseudo-atomic.
                         (when sys.int::*gc-enable-logging*
                           (mezzano.supervisor:debug-print-line "benchmark-shared-cons area expansion failed, performing GC."))
                         (go DO-GC))))))))
     DO-GC
       ;; Must occur outside the locks.
       (when (> gc-count *maximum-allocation-attempts*)
         (cerror "Retry allocation" 'storage-condition))
       (incf gc-count)
       (sys.int::%gc :reason :cons :full (not (eql gc-count 1)))
       (go OUTER-LOOP))))
