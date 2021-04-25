;;;
;;; file: compile.scm
;;;

(define-module (compile)
  #:use-module ((srfi srfi-11))
  #:use-module (common) 
  #:use-module (comb)
  #:export (compile-program
            )
)

;;;
;;; Compiler
;;;
;;; The following procedure recieves a list of definitions and expressions, 
;;; where each definition/expression  is a string. Then it converts each 
;;; definition/expression to an 'object code' which is a list of values 
;;; and procedures where procedures perform combinators. 
;;; Finally it returns a list of object codes converted from expressions.
;;;

(define (compile-program prg)
  (define +defn-bodies+ '())
  (define +expressions+ '())
  (define (generate-definition! name body)
    (let* ((name (string->symbol name))
           (body (joy-exp->Scheme-list body))
           (proc (lambda (stack code) (values stack (append body (cdr code))))))
      (new-combinator name proc)
      (set! +defn-bodies+ (cons body +defn-bodies+))))
  (let loop ((prg prg))
    (if (null? prg)
        'done
        (let* ((exp (car prg))
               (idx (string-contains exp ":=")))
          (check-bracket exp)
          (if idx 
              (let ((name (string-trim-both (substring exp 0 idx) #\space))
                    (body (substring exp (+ idx 2))))
                (generate-definition! name body))
              (set! +expressions+ 
                    (cons (joy-exp->Scheme-list exp) +expressions+)))
          (loop (cdr prg)))))
  (symbol->procedure! +defn-bodies+)
  (symbol->procedure! +expressions+)
  (reverse +expressions+)
)

(define (joy-exp->Scheme-list joy-exp)
  (call-with-input-string 
   (string-map bracket->parenthesis (string-append "(" joy-exp ")"))
   read))

(define (bracket->parenthesis chr)
  (cond
   ((char=? chr #\[) #\()
   ((char=? chr #\]) #\))
   (else chr)))

(define (symbol->procedure! code-list)
  (let loop ((code-list code-list))
    (if (null? code-list)
        'done
        (let ((code (car code-list)))
          (let traverse ((code (car code-list)))
            (cond 
             ((null? code) 'done)
             ((symbol? (car code))
              (let ((proc (assq-ref *op/cmb->proc* (car code))))
                (when proc (set-car! code proc))
                (traverse (cdr code))))
             ((list? (car code))
              (traverse (car code))
              (traverse (cdr code)))
             (else 
              (traverse (cdr code)))))
          (loop (cdr code-list))))))

(define (check-bracket exp)
  (let ((len (string-length exp)))
    (let loop ((idx 0) (counter 0))
      (cond
       ((= idx len) 
        (when (not (= counter 0))
          (error-and-halt exp "brackets are not matched." 'bracket-error)))
       ((eqv? (string-ref exp idx) #\[) 
        (loop (1+ idx) (1+ counter)))
       ((eqv? (string-ref exp idx) #\]) 
        (loop (1+ idx) (1- counter)))
       (else 
        (loop (1+ idx) counter))))))

