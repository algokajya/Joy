;;;
;;; file: common.scm
;;;

(define-module (common)
  #:export (*halt*
            error-and-halt
           ))

(define *halt* (make-undefined-variable))
(define error-and-halt 
  (case-lambda
   ((message rval)
    (display "*Joy-ERROR: ") (display message) (newline)
    (*halt* rval))
   ((exp message rval)
    (display "*Joy-ERROR: ") (display message) (newline)
    (display "+EXP  : ") (write exp) (newline)
    (*halt* rval))
   ((stack code message rval)
    (let ((cmb (assq-ref *proc->op/cmb* proc)))
      (display "*Joy-ERROR: ") (display message) (newline)
      (display "+STACK: ") (write (object->source stack)) (newline)
      (display " |CODE: ") (write (object->source code)) (newline))
    (*halt* rval))))

   
