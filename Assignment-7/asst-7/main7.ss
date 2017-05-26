(eval-when (compile load eval)
           (optimize-level 2)
           (case-sensitive #t))

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")
(load "a7.ss")
(load "a7-wrapper.ss")   ; defines syntactic forms and procedures

(compiler-passes '(
                   verify-scheme
                   remove-complex-opera*
                   flatten-set!
                   impose-calling-conventions
                   uncover-frame-conflict
                   pre-assign-frame
                   assign-new-frame
                   (iterate 
                    finalize-frame-locations
                    select-instructions
                    uncover-register-conflict
                    assign-registers
                    (break when everybody-home?)
                    assign-frame)
                   discard-call-live
                   finalize-locations
                   expose-frame-var
                   expose-basic-blocks
                   flatten-program
                   generate-x86-64
                   ))

(load "tests7.ss")
(tracer #t)
(test-one (list-ref tests 64))
;(test-all #t #t)
