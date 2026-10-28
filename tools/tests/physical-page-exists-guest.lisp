;;;; Exercise the physical-page lookup hint against the actual boot memory map.
(defpackage :mezzano.physical-page-exists-test (:use :cl) (:export :run))
(in-package :mezzano.physical-page-exists-test)

(defun reference-exists-p (page)
  (loop with address = (* page 4096)
        for i below (mezzano.supervisor::n-memory-map-entries)
        thereis (<= (mezzano.supervisor::memory-map-entry-start i)
                    address
                    (1- (mezzano.supervisor::memory-map-entry-end i)))))

(defun check-page (page)
  (assert (eql (mezzano.supervisor::physical-page-exists page)
               (reference-exists-p page)) () "Incorrect physical-page lookup: ~D" page))

(defun run ()
  (let* ((count (mezzano.supervisor::n-memory-map-entries))
         (limit (loop for i below count
                      maximize (ceiling (mezzano.supervisor::memory-map-entry-end i) 4096)))
         (pages (append (list -1 0 limit (1+ limit) most-positive-fixnum)
                        (loop for i below count append
                              (loop for address in (list (mezzano.supervisor::memory-map-entry-start i)
                                                         (mezzano.supervisor::memory-map-entry-end i))
                                    append (let ((page (floor address 4096)))
                                             (list (1- page) page (1+ page))))))))
    ;; An index retained from an older boot with a longer map must fall back.
    (dolist (hint (list 0 count (+ count 7)))
      (setf mezzano.supervisor::*physical-page-exists-hint* hint)
      (dolist (page pages) (check-page page)))
    (let ((done (make-array 4 :initial-element nil))
          (errors (make-array 4 :initial-element nil)))
      (dotimes (index 4)
        (let ((index index))
          (mezzano.supervisor:make-thread
           (lambda ()
             (unwind-protect
                  (handler-case
                      (dotimes (i 10000)
                        (check-page (mod (+ (* i 7919) (* index 104729)) (+ limit 100))))
                    (error (condition) (setf (aref errors index) condition)))
               (setf (aref done index) t)))
           :name "Physical-page hint check")))
      (let ((deadline (+ (get-internal-real-time) (* 30 internal-time-units-per-second))))
        (loop until (every #'identity done)
              do (assert (< (get-internal-real-time) deadline)) (sleep 0.01)))
      (dotimes (i 4) (assert (null (aref errors i)) () "~A" (aref errors i))))
    (format t "~&PHYSICAL-PAGE-EXISTS-TEST-PASSED entries=~D concurrent-lookups=40000~%" count)
    :passed))
