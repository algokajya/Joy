#!/usr/bin/guile \
-e main -s
!#

;;;
;;; file: joy.scm
;;;
;;; Joy is a concatenate programming language invented by Manfred von Thun.
;;; See  http://joy-lang.org/rationale-for-joy/  for the details.  
;;;
;;; This is my practice for learning about Scheme programming using Guile 
;;; but not a serious implementaion of Joy. Thus this implements only a small 
;;; part of Joy. 
;;; 
;;; The dialect implemented by this is slightly different from the original. 
;;;  - All simple values are denoted in Scheme notation such as #t and #f.
;;;  - Some combinators are named differently. For example, 'car' and 'cdr' 
;;;    are used instead of 'first' and 'rest' respectively. 
;;;  - No mode such as DEFINE in the original exists; 
;;;    all definitions and expressions may be arranged in an arbitrary order.
;;;  - A comment line is started with '#' symbol. 
;;;    No block comment is available.
;;;
;;; To use this, run the command
;;;
;;;    $ ./joy.scm <file> [ENTER]
;;; or
;;;    $ ./joy.scm -t <file> [ENTER]
;;;
;;; on a terminal, where '<file>' is a Joy program file. 
;;; Then this command executes the program and prints its result in 
;;; an appropriate form. The '-t' switch specifies to trace execution. 
;;;

;;;
;;; load modules 
;;;
(use-modules (ice-9 textual-ports)
             (ice-9 regex))

(add-to-load-path (getcwd))
(use-modules (common)
             (comb)
             (compile)
             (exec))

;;;
;;; entry point
;;;
(define (main args)
  (call/cc 
   (lambda (cont)
     (define fname "")
     (set! *halt* cont)
     (cond 
      ((= (length args) 2) 
       (set! fname (cadr args)))
      ((= (length args) 3)
       (if (equal? "-t" (cadr args))
         (set! execute-expression execute-with-trace)
         (error-and-halt "illigal arguments." 'args-error))
       (set! fname (caddr args)))
      (else 
       (error-and-halt "illigal arguments." 'args-error)))
     (if (not (file-exists? fname))
         (error-and-halt "file does not exists." 'file-error)
         (joy (file->source-code fname)))
     )))

;;;
;;; This converts a Joy program file to a list of definitions and expressions, 
;;; which I call a 'source code', where each definition or expression is 
;;; a string. 
;;;
(define (file->source-code fname)
  (define str (call-with-input-file fname (lambda (p) (get-string-all p))))
  (set! str (regexp-substitute/global #f "#[^\n]*(\n|$)" str 'pre " " 'post))
  (string-map! (lambda (c) (if (eqv? c #\newline) #\space c)) str)
  (delete! "" (map string-trim-both (string-split str #\;))))

;;;
;;; Joy program evaluator
;;; This compiles a source code into a list of object codes and then 
;;; execute each expression in the list.  
;;;
(define (joy prg)
  (let loop ((exp-list (compile-program prg)))
    (if (null? exp-list) 
        'done
        (begin
          (execute-expression '() (car exp-list))
          (loop (cdr exp-list))))))
    
