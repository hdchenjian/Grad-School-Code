;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2009

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the grammar of the third assignment:
;;;
;;; Grammar assignment 3:
;;;
;;; Program --> (letrec ([<label> (lambda () <Body>)]*) <Body>)
;;; Body    --> (locate ([uvar <Loc>]*) <Tail>)
;;; Tail    --> (<Triv>)
;;;          |  (begin <Effect>* <Tail>)
;;;          |  (if <Pred> <Tail> <Tail>)
;;; Pred    --> (true)
;;;          |  (false)
;;;          |  (predop <Triv> <Triv>)
;;;          |  (begin <Effect>* <Pred>)
;;;          |  (if <Pred> <Pred> <Pred>)
;;; Effect  --> (nop)
;;;          |  (set! <Var> <Triv>)
;;;          |  (set! <Var> (<binop> <Triv> <Triv>)
;;;          |  (begin <Effect>+)
;;;          |  (if <Pred> <Effect> <Effect>)
;;; Var     --> uvar
;;;          |  Loc
;;; Loc     --> register
;;;          |  frame-var
;;; Triv    --> Var
;;;          |  int
;;;          |  label
;;;
;;; Where uvar is symbol.n where (n >= 0)
;;;       binop is +, -, *, logand, logor, or sra
;;;       predop is <, <=, =, >=, or >
;;;       register is rax, rcx, rdx, rbx, rbp, rdi, rsi, r8,
;;;                   r9, r10, r11, r12, r13, r14, or r15
;;;       label is symbol$n where (n >= 0)
;;;       frame-var is fvn where (n >= 0)
;;;
;;; If the value is a valid program, verify scheme returns the value
;;; unchanged; otherwise, it signals an error.

(load "helpers.ss")
(load "match.ss")



(define-who (verify-scheme program)

  #| binop? : sym --> boolean
   | binop? takes a symbol and returns #t 
   | iff the symbol is a binary operation.
   | 
   | Binop --> + | - | * | logand | logor | sra
   |#
  (define (binop? exp)
    (define binops '(+ - * logand logor sra))
    (and (memq exp binops) #t)
  )

  #| var? : exp --> boolean
   | var? takes an expression and returns #t
   | iff the expression is a variable.
   |
   | Var --> ,Register | <frame variable>
   |#
  (define (var? exp)
    (or (register? exp) (frame-var? exp))
  )

  #| triv? : exp --> boolean
   | triv? takes an expression and returns #t
   | iff the expression is trivial.
   |
   | Triv --> ,Var | <integer> | <label>
   |#
  (define (triv? exp)
    (or (var? exp) (integer? exp) (label? exp))
  )

  #| Effect : exp --> void
   | Effect takes an expression and throws an error
   | unless the expression qualifies as an effect.
   |
   | Effect --> (set! ,Var ,Triv)
   |         |  (set! ,Var (,Binop ,Triv ,Triv))
   |#
  (define (Effect exp)
    (match exp
      [(set! ,v ,t)
       (guard (var? v) (triv? t)
              ; architecture specific constraints
              (not (and (frame-var? v) (frame-var? t))) ; v & t cannot both be frame-vars
              (if (label? t) (register? v)) ; labels only fit in registers
              (if (integer? t) (or (int32? t) ; ints must be 32bit or 
                                   (and (register? v) (int64? t)))) ; int64's only fit into registers
       )
       exp]
      [(set! ,v (,b ,t1 ,t2))
       (guard (var? v) (binop? b) (triv? t2)
              ; architecture specific constraints
              (eq? v t1) ; (set! v (b t t0)) :: valid iff v equals t
              (not (or (label? t1) (label? t1))) ; no labels as operands to binops
              (not (and (frame-var? t1) (frame-var? t2))) ; t & t0 cannot both be frame-vars
              ; Integer operands of binary operations must be an exact integer -2^31 ≤ n ≤ 2^31 - 1
              (if (number? t1) (and (int32? t1) (exact? t1)))
              (if (number? t2) (and (int32? t2) (exact? t2)))
              (if (eq? b '*) (register? v)) ; * must go into a register
              (if (eq? b 'sra) (and (<= 0 t2) (>= 63 t2))) ; whatever.
       )
       exp]
      [,x (errorf who "invalid effect: ~s" x)]
    )
  )

  #| Tail : exp --> void
   | Tail takes an expression and throws an error
   | unless the expression qualifies as a tail.
   |
   | Tail --> (,Triv)
   |       |  (begin ,Effect* ,Tail)
   |#
  (define (Tail env)
    (lambda (exp)
      ;(display exp) (newline)
      (match exp
        [(begin ,[Effect -> e*] ... ,[(Tail env) -> t]) exp]
        [(,t) (guard
                (triv? t)
                (if (label? t) (and (member t env) #t))
                (not (integer? t)) ; architectural nuance.  Jump must be to label, not address.
                )
         t]
        [,x (errorf who "invalid tail: ~s" x)]
      )
    )
  )

  #| This block acts as the heartbeat of verify-scheme
   | by doing the work naturally expected to be within
   | some helper 'verify-program'.
   |
   | Program --> (letrec ([<label> (lambda () ,Tail)]*) ,Tail)
   |#
  (match program
    [(letrec ([,lbl (lambda () ,t*)] ...) ,t)
     (let ([env (cons 'r15 lbl)])
       (for-each (Tail env) t*)
       ((Tail env) t)
       (if (set? (map string->number (map extract-suffix lbl)))
           program
           (errorf who "Label suffixes must be unique: ~s" lbl)))]
    [,x (errorf who "invalid syntax for Program: expected (letrec ([<label> (lambda () ,Tail)]*) ,Tail) but received ~s" program)]
  )
)
