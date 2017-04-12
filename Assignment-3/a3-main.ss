 ;Basic Usage:
 ;
 ;To use the driver for Assignment 1, create a file containing:

 (eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
 )

 (load "match.ss")
 (load "helpers.ss")
 (load "fmts.pretty")     ; inform pretty-print about new forms
 (load "driver.ss")

 (load "a3.ss")
 (load "a3-wrapper.ss")   ; defines syntactic forms and procedures
                          ; needed to output of each pass
 (compiler-passes '(
   verify-scheme
   finalize-locations
   expose-frame-var
   expose-basic-blocks
   flatten-program
   generate-x86-64
 ))

(load "tests3.ss")
(tracer #t)
(test-all #t #t)

;;(define test-nth 0)
;;(test-one (list-ref tests 48) #t #t)

(define (test-individual n)
  (if (equal? n 100)
      (void)
      (begin
        (display n)(newline)
        (test-one (list-ref tests n) #t #t)
        (test-individual (+ n 1)))))
;;(test-individual 0)

(define (compiler x)
  (generate-x86-64
   (flatten-program
    (expose-basic-blocks
     (expose-frame-var
      (finalize-locations
       (verify-scheme x)))))))
