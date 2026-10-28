(in-package :mezzano.supervisor)

(defun snapshot-mark-cow-dirty-pages ()
  ;; Mark all non-wired dirty pages as read-only and set their CoW bits.
  (let ((prepare-p t))
    (labels ((mark-level (pml index next-fn)
             ;;(debug-print-line " PML " pml " index " index)
             (let* ((entry (sys.int::memref-unsigned-byte-64 pml index))
                    (next-pml (convert-to-pmap-address
                               (logand entry +arm64-tte-address-mask+))))
               (when (logtest entry +arm64-tte-present+)
                 (dotimes (i 512)
                   (funcall next-fn next-pml i)))))
           (mark-pml4e-cow (pml4 pml4e)
             (mark-level pml4 pml4e #'mark-pml3e-cow))
           (mark-pml3e-cow (pml3 pml3e)
             (mark-level pml3 pml3e #'mark-pml2e-cow))
           (mark-pml2e-cow (pml2 pml2e)
             (mark-level pml2 pml2e #'mark-pml1e-cow))
           (mark-pml1e-cow (pml1 pml1e)
             (let ((entry (sys.int::memref-unsigned-byte-64 pml1 pml1e)))
               ;;(debug-print-line "    PML1e " pml1e "  " pml1 "  " entry)
               (when (logtest entry +arm64-tte-copy-on-write+)
                 (panic "Page table entry marked CoW when it shouldn't be."))
               (when (logtest entry +arm64-tte-dirty+)
                 (let ((frame (ash (pte-physical-address (sys.int::memref-unsigned-byte-64 pml1 pml1e)) -12)))
                   (if prepare-p
                       (snapshot-add-writeback-frame frame)
                       ;; Peers are excluded when the CoW PTE is published.
                       (setf (sys.int::memref-unsigned-byte-64 pml1 pml1e)
                             (make-pte frame :copy-on-write t))))))))
      (declare (dynamic-extent #'mark-level
                               #'mark-pml4e-cow #'mark-pml3e-cow
                               #'mark-pml2e-cow #'mark-pml1e-cow))
      (let ((pml4 (convert-to-pmap-address (logand (%ttbr0-el1) (lognot #xFFF)))))
        (flet ((walk ()
                 ;; Skip wired area, entry 0, and wired stack area, entry 64.
                 (loop for i from 1 below 64 do (mark-pml4e-cow pml4 i))
                 (loop for i from 65 below 256 do (mark-pml4e-cow pml4 i))))
          (declare (dynamic-extent #'walk))
          ;; Store allocation can refill metadata, so do it before begin.
          (walk)
          (setf prepare-p nil)
          (begin-tlb-shootdown)
          (walk)
          (tlb-shootdown-all)
          (finish-tlb-shootdown))))))
