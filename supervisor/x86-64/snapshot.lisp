(in-package :mezzano.supervisor)

(defun snapshot-mark-cow-dirty-pages ()
  ;; Mark all non-wired dirty pages as read-only and set their CoW bits.
  (let ((pml4 (convert-to-pmap-address (logand (sys.int::%cr3) (lognot #xFFF))))
        (prepare-p t))
    (labels ((mark-level (pml index next-fn)
               ;;(debug-print-line " PML " pml " index " index)
               (let* ((entry (sys.int::memref-unsigned-byte-64 pml index))
                      (next-pml (convert-to-pmap-address
                                 (logand entry +x86-64-pte-address-mask+))))
                 (when (and (logtest entry +x86-64-pte-present+)
                            (logtest entry +x86-64-pte-accessed+))
                   (dotimes (i 512)
                     (funcall next-fn next-pml i))
                   ;; Leave the walk intact for the PTE publication pass.
                   (unless prepare-p
                     (setf (sys.int::memref-unsigned-byte-64 pml index)
                           (logand entry (lognot +x86-64-pte-accessed+)))))))
             (mark-pml4e-cow (pml4e)
               (mark-level pml4 pml4e #'mark-pml3e-cow))
             (mark-pml3e-cow (pml3 pml3e)
               (mark-level pml3 pml3e #'mark-pml2e-cow))
             (mark-pml2e-cow (pml2 pml2e)
               (mark-level pml2 pml2e #'mark-pml1e-cow))
             (mark-pml1e-cow (pml1 pml1e)
               (let ((entry (sys.int::memref-unsigned-byte-64 pml1 pml1e)))
                 ;;(debug-print-line "    PML1e " pml1e "  " pml1 "  " entry)
                 (when (logtest entry +x86-64-pte-copy-on-write+)
                   (panic "Page table entry marked CoW when it shouldn't be."))
                 (when (logtest entry +x86-64-pte-dirty+)
                   (if prepare-p
                       ;; Store metadata allocation must precede the rendezvous.
                       (snapshot-add-writeback-frame (ash entry -12))
                       ;; Peers are now excluded; publish read-only CoW PTEs.
                       (setf (sys.int::memref-unsigned-byte-64 pml1 pml1e)
                             (logand (logior entry +x86-64-pte-copy-on-write+)
                                     (lognot +x86-64-pte-write+)
                                     (lognot +x86-64-pte-dirty+))))))))
      (declare (dynamic-extent #'mark-level
                               #'mark-pml4e-cow #'mark-pml3e-cow
                               #'mark-pml2e-cow #'mark-pml1e-cow))
      ;; Prepare the backing store before holding peers in the IPI handler.
      ;; Skip wired area, entry 0.
      (loop for i from 1 below 64 ; pinned area to wired stack area.
         do (mark-pml4e-cow i))
      ;; Skip wired stack area, entry 64.
      (loop for i from 65 below 256 ; stack area to end of persistent memory.
         do (mark-pml4e-cow i))
      (setf prepare-p nil)
      (begin-tlb-shootdown)
      (loop for i from 1 below 64
         do (mark-pml4e-cow i))
      (loop for i from 65 below 256
         do (mark-pml4e-cow i))
      (tlb-shootdown-all)
      (finish-tlb-shootdown))))
