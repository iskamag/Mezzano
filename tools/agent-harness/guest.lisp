;;;; Guest half of the serial agent harness.  Loaded on demand into a live image.
(defpackage :mezzano.agent-harness
  (:use :cl)
  (:export #:submit #:restart-job #:abort-job #:input-job))

(in-package :mezzano.agent-harness)

;; Written only after the entire file loads.  A failed load is retried rather
;; than mistaking a partly defined package for an initialized harness.
(defparameter *protocol-version* nil)

;; Jobs are retained for the life of the image to make duplicate submissions
;; harmless.  A vector avoids changing any existing image structure layout.
(defvar *jobs* (make-hash-table :test 'equal))
(defvar *jobs-lock* (mezzano.supervisor:make-mutex "Agent harness jobs"))
(defconstant +thread+ 0)
(defconstant +lock+ 1)
(defconstant +condition+ 2)
(defconstant +restarts+ 3)
(defconstant +action+ 4)
(defconstant +phase+ 5)
(defconstant +debug-generation+ 6)
(defconstant +input-generation+ 7)

(defun job-path (id name)
  (merge-pathnames (format nil ".mezzano-agent/~A/~A" id name)
                   (user-homedir-pathname)))

(defun mark (id name &optional (value 1))
  ;; Close the marker only after the data it describes has been closed.
  (with-open-file (s (job-path id name) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (format s "~A~%" value)))

(defun read-package-name (id)
  (with-open-file (s (job-path id "package.txt") :direction :input
                     :external-format :utf-8)
    (read-line s)))

(defun describe-debugger (id condition restarts)
  (with-open-file (s (job-path id "debug.txt") :direction :output
                     :if-exists :supersede :if-does-not-exist :create
                     :external-format :utf-8)
    (let ((*print-readably* nil) (*print-level* 8) (*print-length* 30))
      (format s "Condition: ~A~%Type: ~S~%~%Restarts:~%"
              condition (type-of condition))
      (loop for restart in restarts for i from 0
            do (format s "~D: ~A -- ~A~%" i (restart-name restart) restart))
      (format s "~%Backtrace:~%")
      (let ((index 0))
        (mezzano.debug:map-backtrace
         (lambda (frame)
           (when (< index 40)
             (format s "~D: " index)
             (handler-case (mezzano.debug:print-frame frame :stream s)
               (error () (write-string "<unavailable>" s)))
             (terpri s)
             (incf index))))))))

(defun debug-job (id job condition)
  ;; Called in the faulting thread, before unwinding, including for BREAK.
  (let ((restarts (compute-restarts condition))
        (action nil))
    (finish-output *standard-output*)
    (describe-debugger id condition restarts)
    (mezzano.supervisor:with-mutex ((aref job +lock+))
      (setf (aref job +restarts+) restarts
            (aref job +action+) nil
            (aref job +phase+) :debug)
      (incf (aref job +debug-generation+))
      (mark id "debug-ready" (aref job +debug-generation+))
      (unwind-protect
           (loop until (setf action (aref job +action+))
                 do (mezzano.supervisor:condition-wait
                     (aref job +condition+) (aref job +lock+)))
        (ignore-errors (delete-file (job-path id "debug-ready")))
        (setf (aref job +phase+) :running
              (aref job +action+) nil
              (aref job +restarts+) nil)))
    (if (eq action :abort)
        (throw 'abort-agent-job :aborted)
        (invoke-restart (nth (1- action) restarts)))))

(defclass job-input-stream (mezzano.gray:unread-char-mixin
                            mezzano.gray:fundamental-character-input-stream)
  ((id :initarg :id :reader input-id)
   (job :initarg :job :reader input-job-entry)
   (text :initform "" :accessor input-text)
   (position :initform 0 :accessor input-position)
   (eof :initform nil :accessor input-eof)))

(defun await-input (id job)
  (let ((action nil))
    (finish-output *standard-output*)
    (mezzano.supervisor:with-mutex ((aref job +lock+))
      (setf (aref job +action+) nil (aref job +phase+) :input)
      (incf (aref job +input-generation+))
      (mark id "input-ready" (aref job +input-generation+))
      (unwind-protect
           (loop until (setf action (aref job +action+))
                 do (mezzano.supervisor:condition-wait
                     (aref job +condition+) (aref job +lock+)))
        (ignore-errors (delete-file (job-path id "input-ready")))
        (setf (aref job +phase+) :running (aref job +action+) nil)))
    (cond ((eq action :abort) (throw 'abort-agent-job :aborted))
          ((eq action :eof) :eof)
          (t
           (with-open-file (s (job-path id (format nil "input-~A.txt" (cdr action)))
                              :direction :input :external-format :utf-8)
             (with-output-to-string (out)
               (loop for char = (read-char s nil nil)
                     while char do (write-char char out))))))))

(defmethod mezzano.gray:stream-read-char ((stream job-input-stream))
  (loop
    (when (< (input-position stream) (length (input-text stream)))
      (return (prog1 (char (input-text stream) (input-position stream))
                (incf (input-position stream)))))
    (when (input-eof stream) (return :eof))
    (let ((next (await-input (input-id stream) (input-job-entry stream))))
      (if (eq next :eof)
          (setf (input-eof stream) t)
          (setf (input-text stream) next (input-position stream) 0)))))

(defmethod mezzano.gray:stream-read-char-no-hang ((stream job-input-stream))
  (cond ((< (input-position stream) (length (input-text stream)))
         (prog1 (char (input-text stream) (input-position stream))
           (incf (input-position stream))))
        ((input-eof stream) :eof)
        (t nil)))

(defmethod mezzano.gray:stream-clear-input ((stream job-input-stream))
  (setf (input-text stream) "" (input-position stream) 0))

(defmethod mezzano.gray:interactive-stream-p ((stream job-input-stream))
  (declare (ignore stream)) t)

(defun eval-source (id)
  (let* ((package-name (read-package-name id))
         (*package* (or (find-package package-name)
                        (error "No such package: ~S" package-name)))
         (eof (list :eof))
         (values nil))
    (with-open-file (source (job-path id "source.lisp") :direction :input
                            :external-format :utf-8)
      (loop for form = (read source nil eof)
            until (eq form eof)
            do (setf values (multiple-value-list (eval form)))
               (finish-output *standard-output*)))
    (with-open-file (s (job-path id "values.txt") :direction :output
                       :if-exists :supersede :if-does-not-exist :create
                       :external-format :utf-8)
      (let ((*print-readably* nil) (*print-circle* t)
            (*print-length* 100) (*print-level* 12))
        (if values
            (dolist (value values) (write value :stream s) (terpri s))
            (write-line "; No values" s))))))

(defun record-failure (id condition)
  (ignore-errors
    (with-open-file (s (job-path id "failure.txt") :direction :output
                       :if-exists :supersede :if-does-not-exist :create
                       :external-format :utf-8)
      (format s "Harness failure: ~A~%" condition))))

(defun run-job (id job)
  (let ((out nil) (outcome :cancelled)
        (*debugger-hook* nil)
        (mezzano.debug:*global-debugger*
          (lambda (condition)
            ;; If reporting the debugger itself fails, do not enter the global
            ;; debugger on a second thread or claim that the job succeeded.
            (handler-case (debug-job id job condition)
              (error (c)
                (record-failure id c)
                (throw 'abort-agent-job :failed))))))
    (unwind-protect
         (setf outcome
               (catch 'abort-agent-job
                 (mezzano.supervisor:with-mutex ((aref job +lock+))
                   (when (eq (aref job +action+) :abort)
                     (throw 'abort-agent-job :aborted))
                   (setf (aref job +phase+) :running))
                 (let* ((out (setf out (open (job-path id "output.txt")
                                             :direction :output :if-exists :supersede
                                             :if-does-not-exist :create :external-format :utf-8)))
                        (*standard-output* out)
                        (*error-output* out)
                        (*trace-output* out)
                        ;; Reads request input from the host, never the serial listener.
                        (*standard-input* (make-instance 'job-input-stream :id id :job job))
                        (*terminal-io* (make-two-way-stream *standard-input* out))
                        (*query-io* *terminal-io*)
                        (*debug-io* *terminal-io*))
                   (mark id "started")
                   (eval-source id)
                   :ok)))
      (when out
        (handler-case (close out)
          (error (c) (setf outcome :failed) (record-failure id c))))
      (mezzano.supervisor:with-mutex ((aref job +lock+))
        (setf (aref job +phase+) :finished))
      (ignore-errors (delete-file (job-path id "debug-ready")))
      (ignore-errors (delete-file (job-path id "input-ready")))
      (ignore-errors
        (mark id (case outcome
                   (:ok "ok") (:aborted "aborted") (:failed "failed")
                   (t "cancelled"))))
      ;; Keep the ID for idempotence, but do not retain the completed thread,
      ;; streams, mutex, or condition variable indefinitely.
      (mezzano.supervisor:with-mutex (*jobs-lock*)
        (when (eq (gethash id *jobs*) job)
          (setf (gethash id *jobs*) :finished))))))

(defun get-job (id)
  (mezzano.supervisor:with-mutex (*jobs-lock*)
    (gethash id *jobs*)))

(defun submit (id)
  (mezzano.supervisor:with-mutex (*jobs-lock*)
    (when (gethash id *jobs*) (return-from submit :duplicate))
    (let ((job (vector nil
                       (mezzano.supervisor:make-mutex "Agent harness job")
                       (mezzano.supervisor:make-condition-variable "Agent harness debugger")
                       nil nil :starting 0 0)))
      (setf (gethash id *jobs*) job)
      (handler-case
          (setf (aref job +thread+)
                (mezzano.supervisor:make-thread
                 (lambda () (run-job id job)) :name (format nil "Agent job ~A" id)))
        (error (c)
          (remhash id *jobs*)
          (error c)))
      :started)))

(defun restart-job (id index generation)
  (let ((job (get-job id)))
    (unless job (return-from restart-job :unknown-job))
    (when (eq job :finished) (return-from restart-job :not-debugging))
    (mezzano.supervisor:with-mutex ((aref job +lock+))
      (unless (and (eq (aref job +phase+) :debug)
                   (null (aref job +action+)))
        (return-from restart-job :not-debugging))
      (unless (eql generation (aref job +debug-generation+))
        (return-from restart-job :stale-request))
      (unless (and (integerp index) (<= 0 index)
                   (< index (length (aref job +restarts+))))
        (return-from restart-job :invalid-restart))
      ;; Zero is a valid index, so use index+1 as the non-NIL action.
      (setf (aref job +action+) (1+ index))
      (mezzano.supervisor:condition-notify (aref job +condition+) t)
      :requested)))

(defun abort-job (id)
  (let ((job (get-job id)) (thread nil))
    (unless job (return-from abort-job :unknown-job))
    (when (eq job :finished) (return-from abort-job :finished))
    (mezzano.supervisor:with-mutex ((aref job +lock+))
      (case (aref job +phase+)
        (:finished (return-from abort-job :finished))
        (:starting
         (setf (aref job +action+) :abort)
         (return-from abort-job :requested))
        ((:debug :input)
         (when (aref job +action+) (return-from abort-job :already-requested))
         (setf (aref job +action+) :abort)
         (mezzano.supervisor:condition-notify (aref job +condition+) t)
         (return-from abort-job :requested))
        (t (setf thread (aref job +thread+)))))
    ;; Delivering a foothold may wait for the target.  Do not hold the serial
    ;; listener up while waiting for it.
    (mezzano.supervisor:make-thread
     (lambda () (mezzano.supervisor:terminate-thread thread))
     :name (format nil "Abort agent job ~A" id))
    :requested))

(defun input-job (id input-ref generation)
  (let ((job (get-job id)))
    (unless job (return-from input-job :unknown-job))
    (when (eq job :finished) (return-from input-job :not-waiting-input))
    (mezzano.supervisor:with-mutex ((aref job +lock+))
      (unless (and (eq (aref job +phase+) :input)
                   (null (aref job +action+)))
        (return-from input-job :not-waiting-input))
      (unless (eql generation (aref job +input-generation+))
        (return-from input-job :stale-request))
      (unless (or (string= input-ref "EOF")
                  (and (= (length input-ref) 32)
                       (every (lambda (c) (digit-char-p c 16)) input-ref)))
        (return-from input-job :invalid-input))
      (setf (aref job +action+)
            (if (string= input-ref "EOF") :eof (cons :input input-ref)))
      (mezzano.supervisor:condition-notify (aref job +condition+) t)
      :requested)))

(setf *protocol-version* 3)
