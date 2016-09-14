;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(defun sys.int::%%unwind-to (target-special-stack-pointer)
  (declare (sys.int::suppress-ssp-checking))
  (loop (when (eq target-special-stack-pointer (sys.int::%%special-stack-pointer))
          (return))
     (assert (sys.int::%%special-stack-pointer))
     (etypecase (svref (sys.int::%%special-stack-pointer) 1)
       (symbol
        (sys.int::%%unbind))
       (simple-vector
        (sys.int::%%disestablish-block-or-tagbody))
       (function
        (sys.int::%%disestablish-unwind-protect)))))

#+x86-64
(sys.int::define-lap-function values-list ((list)
                                           ((list 0)))
  "Returns the elements of LIST as multiple values."
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:sub64 :rsp 16) ; 2 slots
  (sys.lap-x86:cmp32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  ;; RBX = iterator, (:stack 0) = list.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:mov64 (:stack 0) :r8)
  (:gc :frame :layout #*10)
  ;; ECX = value count.
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Pop into R8.
  ;; If LIST is NIL, then R8 must be NIL, so no need to
  ;; set R8 to NIL in the 0-values case.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r8 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R9.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r9 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R10.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r10 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R11.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r11 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R12.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r12 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 sys.int::+tag-object+)
                               (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (:gc :frame :layout #*10 :multiple-values 0)
  unpack-loop
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:cmp32 :ecx #.(ash (+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5) sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jae too-many-values)
  (sys.lap-x86:mov64 :r13 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (:gc :frame :layout #*10 :multiple-values 1)
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :layout #*10 :multiple-values 0)
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:jmp unpack-loop)
  done
  (sys.lap-x86:leave)
  (:gc :no-frame :multiple-values 0)
  (sys.lap-x86:ret)
  type-error
  (:gc :frame :layout #*10)
  (sys.lap-x86:mov64 :r8 (:stack 0))
  (sys.lap-x86:mov64 :r9 (:constant proper-list))
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in list ~S."))
  (sys.lap-x86:mov64 :r9 (:stack 0))
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2)
  bad-arguments
  (:gc :frame)
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2))

#+arm64
(sys.int::define-lap-function sys.int::values-simple-vector ((simple-vector))
  "Returns the elements of SIMPLE-VECTOR as multiple values."
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :incoming-arguments :rcx :layout #*0)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  ;; Check arg count.
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:b.ne bad-arguments)
  ;; Check type.
  (mezzano.lap.arm64:and :x9 :x0 #b1111)
  (mezzano.lap.arm64:subs :xzr :x9 #.sys.int::+tag-object+)
  (mezzano.lap.arm64:b.ne type-error)
  (mezzano.lap.arm64:ldr :x9 (:object :x0 -1))
  ;; Simple vector object tag is zero.
  (mezzano.lap.arm64:ands :xzr :x9 #.(ash (1- (ash 1 sys.int::+object-type-size+))
                                          sys.int::+object-type-shift+))
  (mezzano.lap.arm64:b.ne type-error)
  ;; Get number of values.
  (mezzano.lap.arm64:adds :x9 :xzr :x9 :lsr #.sys.int::+object-data-shift+)
  (mezzano.lap.arm64:b.eq zero-values)
  (mezzano.lap.arm64:subs :xzr :x9 #.(+ (- mezzano.supervisor::+thread-mv-slots-end+
                                           mezzano.supervisor::+thread-mv-slots-start+)
                                        5))
  (mezzano.lap.arm64:b.cs too-many-values)
  ;; Set up. X6(RBX) = vector, X5(RCX) = number of values loaded so far, X9(RAX) = total number of values.
  (mezzano.lap.arm64:orr :x6 :xzr :x0)
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  ;; Load register values.
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x0 (:object :x6 0))
  (mezzano.lap.arm64:subs :xzr :x9 1)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x1 (:object :x6 1))
  (mezzano.lap.arm64:subs :xzr :x9 2)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x2 (:object :x6 2))
  (mezzano.lap.arm64:subs :xzr :x9 3)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x3 (:object :x6 3))
  (mezzano.lap.arm64:subs :xzr :x9 4)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x4 (:object :x6 4))
  (mezzano.lap.arm64:subs :xzr :x9 5)
  (mezzano.lap.arm64:b.eq done)
  ;; Registers are populated, now unpack into the MV-area
  (mezzano.lap.arm64:add :x12 :x28 #.(+ (- 8 sys.int::+tag-object+)
                                        (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (mezzano.lap.arm64:movz :x10 #.(+ (- 8 sys.int::+tag-object+)
                                    (* 5 8))) ; Current index.
  (:gc :frame :multiple-values 0)
  unpack-loop
  (mezzano.lap.arm64:ldr :x7 (:x6 :x10))
  (mezzano.lap.arm64:str :x7 (:x12))
  (:gc :frame :multiple-values 1)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :multiple-values 0)
  (mezzano.lap.arm64:add :x12 :x12 8)
  (mezzano.lap.arm64:add :x10 :x10 8)
  (mezzano.lap.arm64:subs :xzr :x10 :x9)
  (mezzano.lap.arm64:b.ne unpack-loop)
  done
  (mezzano.lap.arm64:add :sp :x29 0)
  (:gc :frame :multiple-values 0)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :multiple-values 0)
  (mezzano.lap.arm64:ret)
  ;; Special-case 0 values as it requires NIL in X0.
  zero-values
  (:gc :frame)
  (mezzano.lap.arm64:orr :x0 :x26 :xzr)
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  (mezzano.lap.arm64:b done)
  (:gc :frame)
  type-error
  (mezzano.lap.arm64:ldr :x1 (:constant simple-vector))
  (mezzano.lap.arm64:ldr :x7 (:function sys.int::raise-type-error))
  (mezzano.lap.arm64:movz :x5 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 0)
  too-many-values
  (mezzano.lap.arm64:ldr :x0 (:constant "Too many values in simple-vector ~S."))
  (mezzano.lap.arm64:orr :x1 :xzr :x6)
  (mezzano.lap.arm64:ldr :x7 (:function error))
  (mezzano.lap.arm64:movz :x5 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 0)
  bad-arguments
  (mezzano.lap.arm64:ldr :x7 (:function sys.int::raise-invalid-argument-error))
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 0))

#+x86-64
(sys.int::define-lap-function sys.int::values-simple-vector ((simple-vector))
  "Returns the elements of SIMPLE-VECTOR as multiple values."
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Check arg count.
  (sys.lap-x86:cmp64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  ;; Check type.
  (sys.lap-x86:mov8 :al :r8l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-object+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :rax (:object :r8 -1))
  ;; Simple vector object tag is zero.
  (sys.lap-x86:test8 :al #.(ash (1- (ash 1 sys.int::+object-type-size+))
                                sys.int::+object-type-shift+))
  (sys.lap-x86:jnz type-error)
  ;; Get number of values.
  (sys.lap-x86:shr64 :rax #.sys.int::+object-data-shift+)
  (sys.lap-x86:jz zero-values)
  (sys.lap-x86:cmp64 :rax #.(+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5))
  (sys.lap-x86:jae too-many-values)
  ;; Set up. RBX = vector, RCX = number of values loaded so far, RAX = total number of values.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Load register values.
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 (:object :rbx 0))
  (sys.lap-x86:cmp64 :rax 1)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r9 (:object :rbx 1))
  (sys.lap-x86:cmp64 :rax 2)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r10 (:object :rbx 2))
  (sys.lap-x86:cmp64 :rax 3)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r11 (:object :rbx 3))
  (sys.lap-x86:cmp64 :rax 4)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r12 (:object :rbx 4))
  (sys.lap-x86:cmp64 :rax 5)
  (sys.lap-x86:je done)
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 sys.int::+tag-object+)
                               (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (sys.lap-x86:mov32 :edx 5) ; Current value.
  (:gc :frame :multiple-values 0)
  unpack-loop
  (sys.lap-x86:mov64 :r13 (:object :rbx 0 :rdx))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (:gc :frame :multiple-values 1)
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :multiple-values 0)
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:add64 :rdx 1)
  (sys.lap-x86:cmp64 :rdx :rax)
  (sys.lap-x86:jne unpack-loop)
  done
  (sys.lap-x86:leave)
  (:gc :no-frame :multiple-values 0)
  (sys.lap-x86:ret)
  ;; Special-case 0 values as it requires NIL in R8.
  zero-values
  (:gc :frame)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:jmp done)
  (:gc :frame)
  type-error
  (sys.lap-x86:mov64 :r9 (:constant simple-vector))
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in simple-vector ~S."))
  (sys.lap-x86:mov64 :r9 :rbx)
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  bad-arguments
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

(defvar *active-catch-handlers*)
(defun sys.int::%catch (tag fn)
  ;; Catch is used in low levelish code, so must avoid allocation.
  (let ((vec (sys.c::make-dx-simple-vector 3)))
    (setf (svref vec 0) *active-catch-handlers*
          (svref vec 1) tag
          (svref vec 2) (flet ((exit-fn (values)
                                 (return-from sys.int::%catch (values-list values))))
                          (declare (dynamic-extent (function exit-fn)))
                          #'exit-fn))
    (let ((*active-catch-handlers* vec))
      (funcall fn))))

(defun sys.int::%throw (tag values)
  ;; Note! The VALUES list has dynamic extent!
  ;; This is fine, as the exit function calls VALUES-LIST on it before unwinding.
  (do ((current *active-catch-handlers* (svref current 0)))
      ((not current)
       (error 'sys.int::bad-catch-tag-error :tag tag))
    (when (eq (svref current 1) tag)
      (funcall (svref current 2) values))))

(defun sys.int::%coerce-to-callable (object)
  (etypecase object
    (function object)
    (symbol
     ;; Fast-path for symbols.
     (let ((fref (sys.int::%object-ref-t object sys.int::+symbol-function+)))
       (when (not fref)
         (return-from sys.int::%coerce-to-callable
           (fdefinition object)))
       (let ((fn (sys.int::%object-ref-t fref sys.int::+fref-function+)))
         (if (sys.int::%undefined-function-p fn)
             (fdefinition object)
             fn))))))

;; (defun eql (x y)
;;   (or (eq x y)
;;       (and (%value-has-tag-p x +tag-object+)
;;            (%value-has-tag-p y +tag-object+)
;;            (eq (%object-tag x) (%object-tag y))
;;            (<= +first-numeric-object-tag+ (%object-tag x) +last-numeric-object-tag+)
;;            (= x y))))
#+x86-64
(sys.int::define-lap-function eql ((x y))
  "Compare X and Y."
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Check arg count.
  (sys.lap-x86:cmp64 :rcx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:jne BAD-ARGUMENTS)
  ;; EQ test.
  ;; This additionally covers fixnums, characters and single-floats.
  (sys.lap-x86:cmp64 :r8 :r9)
  (sys.lap-x86:jne MAYBE-NUMBER-CASE)
  ;; Objects are EQ.
  (sys.lap-x86:mov32 :r8d t)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret)
  (:gc :frame)
  MAYBE-NUMBER-CASE
  ;; Not EQ.
  ;; Both must be objects.
  (sys.lap-x86:mov8 :al :r8l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-object+)
  (sys.lap-x86:jne OBJECTS-UNEQUAL)
  (sys.lap-x86:mov8 :al :r9l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-object+)
  (sys.lap-x86:jne OBJECTS-UNEQUAL)
  ;; Both are objects.
  ;; Test that both are the same kind of object.
  (sys.lap-x86:mov64 :rax (:object :r8 -1))
  (sys.lap-x86:and8 :al #.(ash (1- (ash 1 sys.int::+object-type-size+))
                               sys.int::+object-type-shift+))
  (sys.lap-x86:mov64 :rdx (:object :r9 -1))
  (sys.lap-x86:and8 :dl #.(ash (1- (ash 1 sys.int::+object-type-size+))
                               sys.int::+object-type-shift+))
  (sys.lap-x86:cmp8 :al :dl)
  (sys.lap-x86:jne OBJECTS-UNEQUAL)
  ;; They must be numbers. Characters were handled above.
  (sys.lap-x86:sub8 :al #.(ash sys.int::+first-numeric-object-tag+
                               sys.int::+object-type-shift+))
  (sys.lap-x86:cmp8 :al #.(ash (- sys.int::+last-numeric-object-tag+
                                  sys.int::+first-numeric-object-tag+)
                               sys.int::+object-type-shift+))
  (sys.lap-x86:ja OBJECTS-UNEQUAL)
  ;; Both are numbers of the same type. Tail-call to generic-=.
  ;; RCX was set to fixnum 2 on entry.
  (sys.lap-x86:mov64 :r13 (:function sys.int::generic-=))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+))
  (:gc :frame)
  OBJECTS-UNEQUAL
  ;; Objects are not EQL.
  (sys.lap-x86:mov32 :r8d nil)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret)
  (:gc :frame)
  BAD-ARGUMENTS
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

(in-package :sys.int)

(defun return-address-to-function (return-address)
  "Convert a return address to a function pointer.
Dangerous! The return address must be kept live as a return address on a
thread's stack if this function is called from normal code."
  ;; Return address must be within the pinned or wired area.
  (assert (< return-address sys.int::*pinned-area-bump*))
  ;; Walk backwards looking for an object header with a function type and
  ;; an appropriate entry point.
  (loop
     with address = (logand return-address -16)
     ;; Be careful when reading to avoid bignums.
     for potential-header-type = (ldb (byte +object-type-size+ +object-type-shift+)
                                      (memref-unsigned-byte-8 address 0))
     do
       (when (and
              ;; Closures never contain code.
              (or (eql potential-header-type +object-tag-function+)
                  (eql potential-header-type +object-tag-funcallable-instance+))
              ;; Check entry point halves individually, avoiding bignums.
              ;; Currently the entry point of every non-closure function
              ;; points to the base-address + 16.
              (eql (logand (+ address 16) #xFFFFFFFF)
                   (memref-unsigned-byte-32 (+ address 8) 0))
              (eql (logand (ash (+ address 16) -32) #xFFFFFFFF)
                   (memref-unsigned-byte-32 (+ address 12) 0)))
         (return (%%assemble-value address sys.int::+tag-object+)))
       (decf address 16)))

(defun map-function-gc-metadata (function function-to-inspect)
  "Call FUNCTION with every GC metadata entry in FUNCTION-TO-INSPECT.
Arguments to FUNCTION:
 start-offset
 framep
 interruptp
 pushed-values
 pushed-values-register
 layout-address
 layout-length
 multiple-values
 incoming-arguments
 block-or-tagbody-thunk
 extra-registers"
  (check-type function function)
  (let* ((fn-address (logand (lisp-object-address function-to-inspect) -16))
         (header-data (%object-header-data function-to-inspect))
         (mc-size (* (ldb (byte +function-machine-code-size+
                                +function-machine-code-position+)
                          header-data)
                     16))
         (n-constants (ldb (byte +function-constant-pool-size+
                                 +function-constant-pool-position+)
                           header-data))
         ;; Address of GC metadata & the length.
         (address (+ fn-address mc-size (* n-constants 8)))
         (length (ldb (byte +function-gc-metadata-size+
                            +function-gc-metadata-position+)
                      header-data))
         ;; Position within the metadata.
         (position 0))
    (flet ((consume (&optional (errorp t))
             (when (>= position length)
               (when errorp
                 (mezzano.supervisor:panic "Corrupt GC info in function " function-to-inspect))
               (return-from map-function-gc-metadata))
             (prog1 (memref-unsigned-byte-8 address position)
               (incf position))))
      (declare (dynamic-extent #'consume))
      (loop (let ((start-offset-in-function 0)
                  flags-and-pvr
                  mv-and-ia
                  (pv 0)
                  (n-layout-bits 0)
                  layout-address)
              ;; Read first byte of address, this is where we can terminate.
              (let ((byte (consume nil))
                    (offset 0))
                (setf start-offset-in-function (ldb (byte 7 0) byte)
                      offset 7)
                (when (logtest byte #x80)
                  ;; Read remaining bytes.
                  (loop (let ((byte (consume)))
                          (setf (ldb (byte 7 offset) start-offset-in-function)
                                (ldb (byte 7 0) byte))
                          (incf offset 7)
                          (unless (logtest byte #x80)
                            (return))))))
              ;; Read flag/pvr byte
              (setf flags-and-pvr (consume))
              ;; Read mv-and-ia
              (setf mv-and-ia (consume))
              ;; Read vs32 pv.
              (let ((shift 0))
                (loop
                   (let ((b (consume)))
                     (when (not (logtest b #x80))
                       (setf pv (logior pv (ash (logand b #x3F) shift)))
                       (when (logtest b #x40)
                         (setf pv (- pv)))
                       (return))
                     (setf pv (logior pv (ash (logand b #x7F) shift)))
                     (incf shift 7))))
              ;; Read vu32 n-layout bits.
              (let ((shift 0))
                (loop
                   (let ((b (consume)))
                     (setf n-layout-bits (logior n-layout-bits (ash (logand b #x7F) shift)))
                     (when (not (logtest b #x80))
                       (return))
                     (incf shift 7))))
              (setf layout-address (+ address position))
              ;; Consume layout bits.
              (incf position (ceiling n-layout-bits 8))
              ;; Decode this entry and do something else.
              (funcall function
                       ;; Start offset in the function.
                       start-offset-in-function
                       ;; Frame/no-frame.
                       (logtest flags-and-pvr #b00001)
                       ;; Interrupt.
                       (logtest flags-and-pvr #b00010)
                       ;; Pushed-values.
                       pv
                       ;; Pushed-values-register.
                       (if (logtest flags-and-pvr #b10000)
                           :rcx
                           nil)
                       ;; Layout-address. Fixnum pointer to virtual memory
                       ;; the inspected function must remain live to keep
                       ;; this valid.
                       layout-address
                       ;; Number of bits in the layout.
                       n-layout-bits
                       ;; Multiple-values.
                       (if (eql (ldb (byte 4 0) mv-and-ia) 15)
                           nil
                           (ldb (byte 4 0) mv-and-ia))
                       ;; Incoming-arguments.
                       (if (logtest flags-and-pvr #b1000)
                           (if (eql (ldb (byte 4 4) mv-and-ia) 15)
                               :rcx
                               (ldb (byte 4 4) mv-and-ia))
                           nil)
                       ;; Block-or-tagbody-thunk.
                       (if (logtest flags-and-pvr #b0100)
                           :rax
                           nil)
                       ;; Extra-registers.
                       (case (ldb (byte 2 6) flags-and-pvr)
                         (0 nil)
                         (1 :rax)
                         (2 :rax-rcx)
                         (3 :rax-rcx-rdx))))))))

#+x86-64
(define-lap-function %copy-words ((destination-address source-address count))
  "Copy COUNT words from SOURCE-ADDRESS to DESTINATION-ADDRESS.
Source & destination must both be byte addresses."
  (sys.lap-x86:mov64 :rdi :r8) ; Destination
  (sys.lap-x86:mov64 :rsi :r9) ; Source
  (sys.lap-x86:mov64 :rcx :r10) ; Count
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+) ; Unbox destination
  (sys.lap-x86:sar64 :rsi #.+n-fixnum-bits+) ; Unbox source
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+) ; Unbox count
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  (sys.lap-x86:ret))

#+x86-64
(define-lap-function %fill-words ((destination-address value count))
  "Store VALUE into COUNT words starting at DESTINATION-ADDRESS.
Destination must a be byte address.
VALUE must be an immediate value (fixnum, character, single-float, NIL or T) or
the GC must be deferred during FILL-WORDS."
  (sys.lap-x86:mov64 :rdi :r8) ; Destination
  (sys.lap-x86:mov64 :rax :r9) ; Value
  (sys.lap-x86:mov64 :rcx :r10) ; Count
  (sys.lap-x86:sar64 :rdi #.+n-fixnum-bits+) ; Unbox destination
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+) ; Unbox count
  (sys.lap-x86:rep)
  (sys.lap-x86:stos64)
  (sys.lap-x86:ret))
