;;;
;;; file: comb.scm
;;;
;;; This file contains all the combinators available in this dialect. 
;;; Each procedure performing a combinator recieves the value stack and 
;;; the code stack, and returns those after peforming the combinator. 
;;;
;;; Adding a new combinator is easy: 
;;;   - make a procedure behaving as above and 
;;;   - register the symbol of the combinator and the procedure 
;;;     with the lists '*op/cmb->proc*' and '*proc->op/cmb*' 
;;;     by using the 'new-combinator' procedure. 
;;;

(define-module (comb)
  #:use-module (srfi srfi-11)
  #:use-module (common) 
  #:use-module (exec)
  #:export (*op/cmb->proc* 
            *proc->op/cmb*
            new-combinator
            )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List of combinators
;;;

(define *op/cmb->proc* '())
(define *proc->op/cmb* '())
(define (new-combinator sym proc)
  (define (add-% sym) 
    (string->symbol (string-append (symbol->string sym) "%")))
  (set! *op/cmb->proc* 
        (cons (cons sym proc)  *op/cmb->proc*))
  (set! *proc->op/cmb* 
        (cons (cons proc (add-% sym))  *proc->op/cmb*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; unary or binary operators
;;;
(define unary-operator%
  (case-lambda 
   ((op)
    (lambda (stack code) 
      (when (null? stack)
        (error-and-halt stack code "stack underflow." 'stack-underflow))
      (let ((a (car stack)))
        (values (cons (op a) (cdr stack)) (cdr code)))))
   ((op type?)
    (lambda (stack code) 
      (when (null? stack)
        (error-and-halt stack code "stack underflow." 'stack-underflow))
      (let ((a (car stack)))
        (when (not (type? a)) 
          (error-and-halt stack code "argument is of illegal type." 'type-error))
        (values (cons (op a) (cdr stack)) (cdr code)))))))

(define binary-operator%
  (case-lambda 
   ((op)
    (lambda (stack code) 
      (when (or (null? stack) (null? (cdr stack)))
        (error-and-halt stack code "stack underflow." 'stack-underflow))
      (let ((b (car stack)) (a (cadr stack)))
        (values (cons (op a b) (cddr stack)) (cdr code)))))
   ((op type1? type2?)
    (lambda (stack code) 
      (when (or (null? stack) (null? (cdr stack)))
        (error-and-halt stack code "stack underflow." 'stack-underflow))
      (let ((b (car stack)) (a (cadr stack)))
        (when (or (not (type1? a)) (not (type2? b)))
          (error-and-halt stack code "illigal arguments" 'type-error))
        (values (cons (op a b) (cddr stack)) (cdr code)))))))
    

;; arithmetic operators
(new-combinator '+ (binary-operator% + number? number?))
(new-combinator '- (binary-operator% - number? number?))
(new-combinator '* (binary-operator% * number? number?))
(new-combinator '/ (binary-operator% / number? number?))
(new-combinator 'div (binary-operator% quotient number? number?))
(new-combinator 'rem (binary-operator% remainder number? number?))
(new-combinator 'mod (binary-operator% modulo number? number?))
(new-combinator 'succ (unary-operator% 1+ number?))
(new-combinator 'pred (unary-operator% 1- number?))
(new-combinator 'abs (unary-operator% abs number?))
(define (sign x) 
  (cond ((zero? x ) 0)
        ((positive? x) 1)
        ((negative? x) -1)))
(new-combinator 'sign (unary-operator% sign number?))
(new-combinator 'min (binary-operator% min number? number?))
(new-combinator 'max (binary-operator% max number? number?))

;; arithmetic predicates
(new-combinator '= (binary-operator% = number? number?))
(new-combinator 
 '!= (binary-operator% (lambda (x y) (not (= x y))) number? number?))
(new-combinator '<= (binary-operator% <= number? number?))
(new-combinator '>= (binary-operator% >= number? number?))
(new-combinator '< (binary-operator% < number? number?))
(new-combinator '> (binary-operator% > number? number?))
(new-combinator 'odd? (unary-operator% odd? number?))
(new-combinator 'even? (unary-operator% even? number?))
(new-combinator 'zero? (unary-operator% zero? number?))
(new-combinator 'positive? (unary-operator% positive? number?))
(new-combinator 'negative? (unary-operator% negative? number?))

;; logical operators
(new-combinator 'and (binary-operator% (lambda (x y) (and x y))))
(new-combinator 'or (binary-operator% (lambda (x y) (or x y))))
(new-combinator 'not (unary-operator% not))

;; list operators
(new-combinator 'car (unary-operator% car pair?))
(new-combinator 'cdr (unary-operator% cdr pair?))
(new-combinator 'cadr (unary-operator% cadr pair?))
(new-combinator 'cddr (unary-operator% cddr pair?))
(new-combinator 'caddr (unary-operator% caddr pair?))
(new-combinator 'cdddr (unary-operator% cdddr pair?))
(new-combinator 'cons (binary-operator% cons))
(new-combinator 'swons (binary-operator% (lambda (x y) (cons y x))))
(new-combinator 'append (binary-operator% append list? list?))
(new-combinator 'null? (unary-operator% null? list?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stack operators
;;;
;;; To execute each of the test codes defined below, 
;;; perform the following on REPL:
;;; 
;;;    guile> (load "joy.scm") [ENTER]
;;;    guile> (joy *xxxx-test-code*) [ENTER]
;;;

;; nop: no operation
(define (nop% stack code)
  (values stack (cdr code)))
(new-combinator 'nop nop%)
(define-public *nop-test-code* '("10 nop 20 nop +"))


;; pop: pop the top element of the value stack.
(define (pop% stack code)
  (when (null? stack)
    (error-and-halt stack code "stack underflow." 'stack-underflow))
  (values (cdr stack) (cdr code)))
(new-combinator 'pop pop%)
(define-public *pop-test-code* '("10 20 30 pop pop"))


;; dup: duplicate the top element on the value stack.
(define (dup% stack code)
  (when (null? stack)
    (error-and-halt stack code "stack underflow." 'stack-underflow))
  (let ((elem (car stack)))
    (values (cons elem stack) (cdr code))))
(new-combinator 'dup dup%)
(define-public *dup-test-code* '("10 dup *"))


;; swap: swap the top two elements on the value stack. 
(define (swap% stack code)
  (when (or (null? stack) (null? (cdr stack)))
    (error-and-halt stack code "stack underflow." 'stack-underflow))
  (let ((a (car stack)) (b (cadr stack)))
    (values (cons b (cons a (cddr stack))) (cdr code))))
(new-combinator 'swap swap%)
(define-public *swap-test-code* '("10 20 swap"))

;; unquote: unquote the top element on the value stack. 
(define (unquote% stack code)
  (when (null? stack)
    (error-and-halt stack code "stack underflow." 'stack-underflow))
  (when (not (list? (car stack)))
      (error-and-halt stack code "the top element must be a list." 
                      'unquote-error))
  (let ((lst (car stack)))
    (values (cdr stack) (append lst (cdr code)))))
(new-combinator 'i unquote%)
(new-combinator 'unquote unquote%)
(define-public *unquote-test-code* 
  '("10 20 + [10 20 *] unquote +"
    "10 20 + [10 20 *] i +"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ifte: conditional
;;;
(define (ifte% stack code)
  (when (or (null? stack) (null? (cdr stack)) (null? (cddr stack)))
    (error-and-halt stack code "stack underflow." 'stack-underflow))
  (let ((else-part (car stack)) 
        (then-part (cadr stack))
        (if-part (caddr stack))
        (stack (cdddr stack))
        (code (cdr code)) )
    (when (not (list? if-part))
      (error-and-halt if-part "if-part must be a list." 'if-part-error))
    (when (not (list? then-part))
      (error-and-halt then-part "then-part must be a list." 'if-part-error))
    (when (not (list? else-part))
      (error-and-halt else-part "else-part must be a list." 'if-part-error))
    (if (execute-expression stack if-part)
        (values stack (append then-part code))
        (values stack (append else-part code)))))
(new-combinator 'ifte ifte%)
(define-public *ifte-test-code* 
  '("10 [10 =] [10 *] [20 +] ifte"
    "20 [10 =] [10 *] [20 +] ifte"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; print: print the top element on the value stack. 
;;;
(define (print% stack code) 
  (when (null? stack)
    (error-and-halt stack code "stack underflow." 'stack-underflow))
  (let ((elem (car stack)))
    (display elem) 
    (values (cdr stack) (cdr code))))
(new-combinator 'print print%)
(define-public *print-test-code* '("10 20 print print"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; newline: print a newline.
;;;
(define (newline% stack code) 
  (newline)
  (values stack (cdr code)))
(new-combinator 'newline newline%)
(define-public *newline-test-code* 
  '("10 20 print newline print newline"))


