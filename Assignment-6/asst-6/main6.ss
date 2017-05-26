(eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a6.ss")
(load "a6-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass
(compiler-passes '(
  verify-scheme
  remove-complex-opera*
  flatten-set!
  impose-calling-conventions
))

(load "tests6.ss")
(tracer #t)


(test-one (list-ref tests 0))
;;(test-all)
(test-all #t #t)
