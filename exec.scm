;;;
;;; file: exec.scm
;;;

(define-module (exec)
  #:use-module ((srfi srfi-11))
  #:use-module (common) 
  #:use-module (comb) 
  #:export ( execute-without-trace
             execute-with-trace
             execute-expression
             object->source
             display-configuration
             )
)

;;;
;;; trace
;;;
(define *depth* -1)
(define *indent* "   ")

;;;
;;; The following procedure executes an object code which is a list
;;; of values and procedures performing combinators. 
;;;
;;; A configuration at any time during the execution of the obejct code 
;;; consists of the value stack and the code stack, which are denoted by 
;;; 'stack' and 'code' respectively. When executing the obejct code, 
;;; each value in it is pushed into 'stack' and each procedure  
;;; is called for the configuration at that time. 
;;;

(define (execute-without-trace stack object-code)
  (set! *depth* (1+ *depth*))
  (when (= *depth* 0)
    (display (source-list->source-string (object->source object-code)))
    (newline))
  (let loop ((stack stack) (code object-code))
    (if (null? code) 
        (begin
          (when (= *depth* 0)
            (display "==> ") 
            (display-results stack))
          (set! *depth* (1- *depth*))
          (if (null? stack)
              #f
              (car stack)))
        (let ((term (car code)))
          (if (procedure? term) 
              (let-values (((stack code) (term stack code)))
                (loop stack code))
              (loop (cons term stack) (cdr code)))))))

(define (execute-with-trace stack object-code)
  (set! *depth* (1+ *depth*))
  (begin
    (display-indent)
    (display (source-list->source-string (object->source object-code)))
    (display " ==> ... ") (newline))
  (let loop ((stack stack) (code object-code))
    (display-configuration stack code)
    (if (null? code) 
        (begin
          (begin 
            (display-indent) 
            (display "==> ") 
            (display-results stack))
          (set! *depth* (1- *depth*))
          (if (null? stack)
              #f
              (car stack)))
        (let ((term (car code)))
          (if (procedure? term) 
              (let-values (((stack code) (term stack code)))
                (loop stack code))
              (loop (cons term stack) (cdr code)))))))

(define execute-expression execute-without-trace)

;;;
;;; Utilities for tracing
;;;
(define (object->source object-code)
  (let loop ((scode '()) (ocode object-code))
    (if (null? ocode)
        (reverse scode)
        (let ((term (car ocode)) (ocode (cdr ocode)))
          (cond 
           ((list? term)
            (loop (cons (object->source term) scode) ocode))
           ((procedure? term)
            (loop (cons (assq-ref *proc->op/cmb* term) scode) ocode))
           (else (loop (cons term scode) ocode)))))))

(define (source-list->source-string source-code)
  (let ((str (call-with-output-string (lambda (p) (write source-code p)))))
    (set! str (substring str 1 (1- (string-length str))))
    (set! str (string-map parenthesis->bracket str))
    str))

(define (display-configuration stack code)
  (let ((stack-src (object->source stack))
        (code-src (object->source code)))
    (display-indent)
    (display "*STACK: ") 
    (display (source-list->source-string (reverse stack-src)))
    (newline)
    (display-indent)
    (display " |CODE: ") 
    (display (source-list->source-string code-src) )
    (newline)))

(define (display-results stack)
  (display (source-list->source-string (object->source (reverse stack))))
  (newline))

(define (display-indent)
  (let loop ((depth *depth*))
    (when (> depth 0) (display *indent*) (loop (1- depth)))))
  
(define (parenthesis->bracket chr)
  (cond
   ((char=? chr #\() #\[)
   ((char=? chr #\)) #\])
   (else chr)))
