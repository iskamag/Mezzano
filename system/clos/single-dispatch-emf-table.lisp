;;;; Faster hash table for doing single-dispatch class to effective method lookup.

(in-package :mezzano.clos)

(defstruct (single-dispatch-emf-table
             (:constructor %make-single-dispatch-emf-table (update-lock))
             :slot-locations)
  update-lock
  (table (make-fast-class-hash-table)))

(defun make-single-dispatch-emf-table (generic-function)
  (%make-single-dispatch-emf-table (mezzano.supervisor:make-mutex `(single-dispatch-emf-cache ,generic-function))))

(defun single-dispatch-emf-table-count (emf-table)
  (fast-class-hash-table-count
   (single-dispatch-emf-table-table emf-table)))

(defun single-dispatch-emf-entry (emf-table class)
  (fast-class-hash-table-entry
   (single-dispatch-emf-table-table emf-table)
   class))

(defun #+x86-64 single-dispatch-emf-entry-by-object-slow
       #-x86-64 single-dispatch-emf-entry-by-object (emf-table object)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type single-dispatch-emf-table emf-table))
  (let ((table (single-dispatch-emf-table-table emf-table)))
    (if (sys.int::instance-or-funcallable-instance-p object)
        (let* ((layout (sys.int::%instance-layout object))
               (new-instance (sys.int::layout-new-instance layout)))
          (declare (type sys.int::layout layout))
          (if new-instance
              (let ((layout (sys.int::%instance-layout new-instance)))
                (declare (type sys.int::layout layout))
                (fast-class-hash-table-entry-known-hash
                 table
                 (sys.int::layout-class layout)
                 (sys.int::layout-hash layout)))
              (fast-class-hash-table-entry-known-hash
               table
               (sys.int::layout-class layout)
               (sys.int::layout-hash layout))))
        ;; If it's not an instance, then it must be a built-in class
        (let* ((class (built-in-class-of object))
               ;; This is effectively inlining safe-class-hash when we know
               ;; class is a built-in-class.
               (hash (sys.int::%object-ref-t
                      class (mezzano.runtime::location-offset-t
                             *built-in-class-hash-location*))))
              (fast-class-hash-table-entry-known-hash
               table
               class
               hash)))))

;; Fuse layout extraction and the existing class-cache probe. The storage and
;; weak-key representations are unchanged, so ordinary cache mutation and
;; invalidation apply. Built-in values and superseded instances use the Lisp
;; path. Keep the argument value registers intact until no fallback is needed.
;; Raw hash/offset registers hold no interior heap pointers across a safepoint.
#+x86-64
(sys.int::define-lap-function single-dispatch-emf-entry-by-object ((table object))
  (:gc :no-frame :incoming-arguments :rcx :layout #*0)
  (mezzano.lap.x86:cmp64 :rcx 4)
  (mezzano.lap.x86:jne slow)
  (mezzano.lap.x86:lea64 :rax (:object :r9 -1))
  (mezzano.lap.x86:test64 :rax #xf)
  (mezzano.lap.x86:jnz slow)
  (mezzano.lap.x86:mov8 :al (:object :r9 -1))
  (mezzano.lap.x86:and8 :al #.(logand #xff
                                 (lognot (logior 3 (ash (logxor sys.int::+object-tag-instance+
                                                               sys.int::+object-tag-funcallable-instance+)
                                                       sys.int::+object-type-shift+)))))
  (mezzano.lap.x86:cmp8 :al #.(ash sys.int::+object-tag-instance+ sys.int::+object-type-shift+))
  (mezzano.lap.x86:jne slow)
  (mezzano.lap.x86:mov64 :r11 (:object :r9 -1))
  (mezzano.lap.x86:shr64 :r11 #.sys.int::+object-data-shift+)
  (mezzano.lap.x86:cmp64 (:object-location :r11 #.sys.int::+layout-new-instance+) nil)
  (mezzano.lap.x86:jne slow)
  (mezzano.lap.x86:mov64 :r10 (:object-location :r11 #.sys.int::+layout-class+))
  (mezzano.lap.x86:mov64 :r13 (:object-location :r11 #.sys.int::+layout-hash+))
  ;; SINGLE-DISPATCH-EMF-TABLE.TABLE and FAST-CLASS-HASH-TABLE.TABLE.
  (mezzano.lap.x86:mov64 :r11 (:object-location :r8 #.+single-dispatch-emf-table-table+))
  (mezzano.lap.x86:mov64 :r11 (:object-location :r11 #.+fast-class-hash-table-table+))
  (mezzano.lap.x86:cmp64 :r11 nil)
  (mezzano.lap.x86:je miss)
  (mezzano.lap.x86:mov8 :al (:object :r11 -1))
  (mezzano.lap.x86:and8 :al #xfc)
  (mezzano.lap.x86:cmp8 :al #.(ash sys.int::+object-tag-weak-pointer+ 2))
  (mezzano.lap.x86:jne hashed)
  (mezzano.lap.x86:cmp64 :r10 (:object :r11 #.sys.int::+weak-pointer-key+))
  (mezzano.lap.x86:jne miss)
  (mezzano.lap.x86:mov64 :r8 (:object :r11 #.sys.int::+weak-pointer-value+))
  (mezzano.lap.x86:mov32 :ecx 2)
  (mezzano.lap.x86:ret)
  hashed
  (mezzano.lap.x86:mov64 :rdi (:object :r11 -1))
  (mezzano.lap.x86:shr64 :rdi #.sys.int::+object-data-shift+)
  (mezzano.lap.x86:sub64 :rdi 1)
  (mezzano.lap.x86:mov64 :rsi :r13)
  (mezzano.lap.x86:sar64 :rsi #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.x86:mov32 :eax :esi)
  probe
  (mezzano.lap.x86:mov64 :rdx :rax)
  (mezzano.lap.x86:and64 :rdx :rdi)
  (mezzano.lap.x86:mov64 :r12 (:r11 (:rdx 8) -1))
  (mezzano.lap.x86:cmp64 :r12 nil)
  (mezzano.lap.x86:je miss)
  (mezzano.lap.x86:cmp64 :r12 t)
  (mezzano.lap.x86:je collision)
  (mezzano.lap.x86:cmp64 :r10 (:object :r12 #.sys.int::+weak-pointer-key+))
  (mezzano.lap.x86:jne collision)
  (mezzano.lap.x86:mov64 :r8 (:object :r12 #.sys.int::+weak-pointer-value+))
  (mezzano.lap.x86:mov32 :ecx 2)
  (mezzano.lap.x86:ret)
  collision
  (mezzano.lap.x86:lea64 :rax (:rax (:rax 4) 1))
  (mezzano.lap.x86:add64 :rax :rsi)
  (mezzano.lap.x86:mov32 :eax :eax)
  (mezzano.lap.x86:sar64 :rsi 5)
  (mezzano.lap.x86:jmp probe)
  miss
  (mezzano.lap.x86:mov64 :r8 :r14)
  (mezzano.lap.x86:mov32 :ecx 2)
  (mezzano.lap.x86:ret)
  slow
  (mezzano.lap.x86:jmp (:named-call single-dispatch-emf-entry-by-object-slow)))

(defun (setf single-dispatch-emf-entry) (value emf-table class)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (setf (fast-class-hash-table-entry
           (single-dispatch-emf-table-table emf-table)
           class)
          value)))

(defun clear-single-dispatch-emf-table (emf-table)
  (mezzano.supervisor:with-mutex ((single-dispatch-emf-table-update-lock emf-table))
    (clear-fast-class-hash-table (single-dispatch-emf-table-table emf-table))))
