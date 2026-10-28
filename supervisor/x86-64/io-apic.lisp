(in-package :mezzano.supervisor)

;; IO-APIC MMIO registers.
(defconstant +io-apic-reg-index+ 0)
(defconstant +io-apic-reg-data+  #x10)

;; IO-APIC register offsets.
(defconstant +io-apic-id+            #x00)
(defconstant +io-apic-version+       #x01)
(defconstant +io-apic-redirection+   #x10)

;; Redirection entry bit fields.
(defconstant +io-apic-entry-mask+            #x00010000)
(defconstant +io-apic-entry-trigger-mode+    #x00008000)
(defconstant +io-apic-entry-polarity+        #x00002000)
(defconstant +io-apic-entry-destination-mode+ #x00000800)
(defconstant +io-apic-entry-delivery-mode+   #x00000700)
(defconstant +io-apic-entry-vector+          #x000000FF)

(defconstant +io-apic-base-vector+ 48)

;; Device interrupt vectors are allocated above +IO-APIC-BASE-VECTOR+.
;; The IPI vectors (#x80 wakeup, #x81 panic, #x82 quiesce, #x83 tlb-shootdown,
;; #x85 reschedule, ...) live in the gap below.  To keep the GSI->vector map
;; bijective (so the handler can recover the GSI from the vector) the range
;; #x80..#x8F is skipped: GSIs whose naive vector would land there are shifted
;; up past it.
(defconstant +io-apic-ipi-gap-start+ #x80)
(defconstant +io-apic-ipi-gap-end+   #x90)
(defconstant +io-apic-last-vector+ #xFC)
(defconstant +io-apic-max-gsi+ (- +io-apic-last-vector+ +io-apic-base-vector+
                                  (- +io-apic-ipi-gap-end+ +io-apic-ipi-gap-start+)))

(declaim (inline gsi->vector vector->gsi))
(defun gsi->vector (gsi)
  (let ((v (+ +io-apic-base-vector+ gsi)))
    (if (>= v +io-apic-ipi-gap-start+)
        (+ v (- +io-apic-ipi-gap-end+ +io-apic-ipi-gap-start+))
        v)))

(defun vector->gsi (vector)
  (cond ((>= vector +io-apic-ipi-gap-end+)
         (- vector +io-apic-base-vector+
            (- +io-apic-ipi-gap-end+ +io-apic-ipi-gap-start+)))
        ((< vector +io-apic-ipi-gap-start+)
         (- vector +io-apic-base-vector+))
        (t
         ;; Vector fell in the reserved IPI gap; no valid GSI.
         -1)))

(sys.int::defglobal *io-apics* nil)
(sys.int::defglobal *io-apic-active-p* nil)
(sys.int::defglobal *io-apic-irqs* nil)
(sys.int::defglobal *isa-irq-to-gsi* nil)
(sys.int::defglobal *gsi-flags* nil)

(defstruct (io-apic
             (:area :wired))
  (id 0 :type (unsigned-byte 8))
  (gsi-base 0 :type (unsigned-byte 32))
  (address 0 :type (unsigned-byte 64))
  (mmio-base 0)
  (max-redirection 0 :type (unsigned-byte 8))
  ;; Acquired with interrupts disabled, inside any per-IRQ lock.
  (lock (place-spinlock-initializer)))

(defun io-apic-read-unlocked (apic offset)
  (setf (physical-memref-unsigned-byte-32
         (+ (io-apic-mmio-base apic) +io-apic-reg-index+))
        offset)
  (physical-memref-unsigned-byte-32
   (+ (io-apic-mmio-base apic) +io-apic-reg-data+)))

(defun io-apic-write-unlocked (apic offset value)
  (setf (physical-memref-unsigned-byte-32
         (+ (io-apic-mmio-base apic) +io-apic-reg-index+))
        offset)
  (setf (physical-memref-unsigned-byte-32
         (+ (io-apic-mmio-base apic) +io-apic-reg-data+))
        value))

(defun io-apic-read (apic offset)
  (safe-without-interrupts (apic offset)
    (with-place-spinlock ((io-apic-lock apic))
      (io-apic-read-unlocked apic offset))))

(defun io-apic-write-redirection-unlocked (apic entry value)
  (let ((low (+ +io-apic-redirection+ (* entry 2))))
    ;; Keep the entry masked while its destination half is replaced.
    (io-apic-write-unlocked apic low
                            (logior (ldb (byte 32 0) value) +io-apic-entry-mask+))
    (io-apic-write-unlocked apic (1+ low) (ldb (byte 32 32) value))
    (io-apic-write-unlocked apic low (ldb (byte 32 0) value))))

(defun io-apic-write-redirection (apic entry value)
  (safe-without-interrupts (apic entry value)
    (with-place-spinlock ((io-apic-lock apic))
      (io-apic-write-redirection-unlocked apic entry value))))

(defun io-apic-set-mask (apic entry masked)
  (safe-without-interrupts (apic entry masked)
    (with-place-spinlock ((io-apic-lock apic))
      ;; IRQ delivery/EOI must not construct a 64-bit redirection value:
      ;; a high destination ID would allocate a bignum in interrupt context.
      (let* ((offset (+ +io-apic-redirection+ (* entry 2)))
             (low (io-apic-read-unlocked apic offset)))
        (io-apic-write-unlocked apic offset
                                (if masked
                                    (logior low +io-apic-entry-mask+)
                                    (logand low (lognot +io-apic-entry-mask+))))))))

(defun io-apic-mask-irq (gsi)
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (<= entry (io-apic-max-redirection apic)))
        (io-apic-set-mask apic entry t)
        (return t)))))

(defun io-apic-unmask-irq (gsi)
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (<= entry (io-apic-max-redirection apic)))
        (io-apic-set-mask apic entry nil)
        (return t)))))

(defun io-apic-find-entry (gsi)
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (<= entry (io-apic-max-redirection apic)))
        (return (values apic entry))))))

(defun io-apic-irq-spurious-p (gsi)
  (declare (ignore gsi))
  nil)

(defun gsi-flags-polarity (flags)
  (if (eql (ldb (byte 2 0) flags) 3) :low :high))

(defun gsi-flags-trigger (flags)
  (if (eql (ldb (byte 2 2) flags) 3) :level :edge))

(defun process-isa-overrides (controllers n-controllers)
  (let ((mapping (sys.int::make-simple-vector 16 :wired)))
    (dotimes (i 16) (setf (svref mapping i) i))
    (dotimes (i n-controllers)
      (let ((entry (svref controllers i)))
        (when (acpi-madt-interrupt-source-override-p entry)
          (let ((bus (acpi-madt-interrupt-source-override-bus entry))
                (source (acpi-madt-interrupt-source-override-source entry))
                (gsi (acpi-madt-interrupt-source-override-global-system-interrupt entry))
                (flags (acpi-madt-interrupt-source-override-flags entry)))
            (when (and (eql bus 0) (< source 16))
              (setf (svref mapping source) gsi)
              (when (< gsi 256)
                (setf (svref *gsi-flags* gsi) flags))
              (debug-print-line "MADT override: ISA IRQ " source " -> GSI " gsi))))))
    (setf *isa-irq-to-gsi* mapping)))

(defun init-one-io-apic (entry bsp-apic-id)
  (let* ((phys-addr (acpi-madt-ioapic-address entry))
         (id (acpi-madt-ioapic-id entry))
         (gsi-base (acpi-madt-ioapic-global-system-interrupt-base entry)))
    (map-physical-memory-early (align-down phys-addr +4k-page-size+)
                               +4k-page-size+ "IO-APIC")
    (let* ((apic (make-io-apic :id id :gsi-base gsi-base
                               :address phys-addr :mmio-base phys-addr))
           (max-redir (ldb (byte 8 16) (io-apic-read apic +io-apic-version+)))
           (n-entries (1+ max-redir)))
      (when (> (+ gsi-base max-redir) +io-apic-max-gsi+)
        (debug-print-line "IO-APIC GSI range exceeds supported maximum " +io-apic-max-gsi+)
        (return-from init-one-io-apic nil))
      (dotimes (e n-entries)
        (when (io-apic-find-entry (+ gsi-base e))
          (debug-print-line "Overlapping IO-APIC GSI range at " (+ gsi-base e))
          (return-from init-one-io-apic nil)))
      (setf (io-apic-max-redirection apic) max-redir)
      (push-wired apic *io-apics*)
      (dotimes (e n-entries)
        (io-apic-write-redirection apic e +io-apic-entry-mask+)
        (let* ((gsi (+ gsi-base e))
               (vector (gsi->vector gsi))
               (flags (if (< gsi 256) (svref *gsi-flags* gsi) 0)))
          (io-apic-configure-entry gsi vector bsp-apic-id
                                   :trigger-mode (gsi-flags-trigger flags)
                                   :polarity (gsi-flags-polarity flags)
                                   :masked t)
          (setf (svref *io-apic-irqs* gsi) (make-irq :platform-number gsi))))
      (debug-print-line "IO-APIC " id " at " phys-addr
                        " GSI base " gsi-base " max redirect " max-redir)
      t)))

(defun init-io-apic-controllers (controllers n-controllers)
  (setf *io-apic-irqs* (sys.int::make-simple-vector (1+ +io-apic-max-gsi+) :wired))
  ;; Raw wired vector allocation leaves zero fixnums, not NIL pointers.
  (dotimes (gsi (sys.int::simple-vector-length *io-apic-irqs*))
    (setf (svref *io-apic-irqs* gsi) nil))
  (setf *io-apics* '())
  (let ((bsp-id (if (boundp '*bsp-cpu*) (x86-64-cpu-apic-id *bsp-cpu*) 0)))
    (when (> bsp-id #xff)
      (debug-print-line "IO-APIC cannot target BSP APIC ID " bsp-id)
      (return-from init-io-apic-controllers nil))
    (dotimes (i n-controllers)
      (let ((entry (svref controllers i)))
        (when (acpi-madt-ioapic-p entry)
          (unless (init-one-io-apic entry bsp-id)
            (setf *io-apics* nil)
            (return-from init-io-apic-controllers nil))))))
  t)

(defun hook-io-apic-interrupt-handlers ()
  (debug-print-line "IO-APIC init done. Hooking handlers...")
  (dotimes (gsi (sys.int::simple-vector-length *io-apic-irqs*))
    (when (svref *io-apic-irqs* gsi)
      (hook-user-interrupt (gsi->vector gsi) 'io-apic-interrupt-handler))))

(defun finalize-io-apic-init ()
  (when (boundp '*i8259-shadow-mask*)
    (setf (sys.int::io-port/8 #x21) #xFF
          (sys.int::io-port/8 #xA1) #xFF
          *i8259-shadow-mask* #xFFFF))
  (setf (lapic-reg +lapic-reg-lvt-lint0+)
        (logior (lapic-reg +lapic-reg-lvt-lint0+) +lapic-lvt-mask+)
        (lapic-reg +lapic-reg-lvt-lint1+)
        (logior (lapic-reg +lapic-reg-lvt-lint1+) +lapic-lvt-mask+))
  (setf *io-apic-active-p* t))

(defun initialize-io-apics ()
  (setf *io-apic-active-p* nil)
  ;; Rebuild controller and IRQ objects each boot; attachments belong to the
  ;; previous boot's IRQ objects, as with initialize-i8259.
  (setf *io-apics* nil *io-apic-irqs* nil *isa-irq-to-gsi* nil *gsi-flags* nil)
  (let ((madt (acpi-get-table 'acpi-madt-table-p)))
    (unless madt
      (debug-print-line "No MADT table, IO-APIC not available.")
      (return-from initialize-io-apics))
    (setf *gsi-flags* (sys.int::make-simple-vector 256 :wired))
    (dotimes (gsi 256)
      (setf (svref *gsi-flags* gsi) 0))
    (let* ((controllers (acpi-madt-table-controllers madt))
           (n (sys.int::simple-vector-length controllers)))
      (process-isa-overrides controllers n)
      (when (and (init-io-apic-controllers controllers n)
                 *io-apics*
                 (loop for source below 16
                       for gsi = (svref *isa-irq-to-gsi* source)
                       always (and (<= gsi +io-apic-max-gsi+)
                                   (svref *io-apic-irqs* gsi))))
        (hook-io-apic-interrupt-handlers)
        (finalize-io-apic-init))
      (unless *io-apic-active-p*
        (debug-print-line "IO-APIC routing unavailable; using i8259.")))))

(defun io-apic-interrupt-handler (interrupt-frame info)
  (let ((gsi (vector->gsi info)))
    (when (and (<= 0 gsi +io-apic-max-gsi+)
               (svref *io-apic-irqs* gsi))
      (irq-deliver interrupt-frame (svref *io-apic-irqs* gsi)))
    (lapic-eoi)
    (maybe-preempt-via-interrupt interrupt-frame)))

(defun io-apic-configure-entry (gsi vector destination-apic-id
                                &key (trigger-mode :edge) (polarity :high) (masked t))
  (multiple-value-bind (apic entry) (io-apic-find-entry gsi)
    (unless apic
      (debug-print-line "No IO-APIC for GSI " gsi)
      (return-from io-apic-configure-entry nil))
    (unless (<= 0 destination-apic-id #xff)
      (debug-print-line "IO-APIC destination APIC ID unsupported: " destination-apic-id)
      (return-from io-apic-configure-entry nil))
    (let ((entry-value (logior vector
                               (if masked +io-apic-entry-mask+ 0)
                               (ecase trigger-mode
                                 (:edge 0)
                                 (:level +io-apic-entry-trigger-mode+))
                               (ecase polarity
                                 (:high 0)
                                 (:low +io-apic-entry-polarity+)))))
      ;; Legacy IO-APIC routing has an eight-bit physical destination.
      (setf entry-value (logior entry-value
                                (ash (ldb (byte 8 0) destination-apic-id) 56)))
      (io-apic-write-redirection apic entry entry-value)
      t)))
