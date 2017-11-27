#lang sicp

(define (square x)
  (* x x))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z)
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z)
     (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; ex 2.73

;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;          (make-product
;           (multiplier exp)
;           (deriv (multiplicand exp) var))
;          (make-product
;           (deriv (multiplier exp) var)
;           (multiplicand exp))))
;        (else (error "unknown expression type: DERIV" exp))))

;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0))
;        (else ((get 'deriv (operator exp))
;               (operands exp)
;               var))))
;
;(define (operator exp) (car exp))
;(define (operand exp) (cdr exp))

;; the rewrite of the code tries to assimilate the procedure using a
;; data-directed approach.
;; 1. those procedures do not fall under the table of procedures,
;; theres nothing to dispatch

; 2.

;(define (install-sum-package)
;  (define (make-sum a1 a2) (cons a1 a2))
;  (define (addend s) (cadr s))
;  (define (augend s) (caddr s))
;  (define (deriv-sum s)
;    (make-sum (deriv (addend s))
;              (deriv (augend s))))
;  (define (tag x) (attach-tag '+ x))
;  (put 'deriv '(+) deriv-sum)
;  (put 'make-sum '+
;       (lambda (x y)
;         (tag (make-sum x y))))
;  'done)
;
;(define (install-product-package)
;  (define (make-product m1 m2) (cons m1 m2))
;  (define (multiplier p) (cadr p))
;  (define (multiplicand p) (caddr p))
;  (define (deriv-product p)
;    (make-sum
;     (make-product (multiplier exp)
;                   (deriv (multiplicand exp) var))
;     (make-product (deriv (multiplier exp) var)
;                   (multiplicand exp))))
;  (define (tag x) (attach-tag '* x))
;  (put 'deriv '(*) deriv-product)
;  (put 'make-product '*
;       (lambda (x y)
;         (tag (make-product x y))))
;  'done)

; 3.

;(define (exponentiation-deriv expr var)
;  (make-product (exponent expr)
;                (make-product
;                 (make-exponentiation (bae expr)
;                                      (make-sum (exponent expr) -1))
;                 (deriv (base expr) var))))
;(define (exponent expr)
;  (cadr expr))
;(define (base expr)
;  (car expr))
;(define (make-exponentiation base exponent)
;  (cond ((=number? exponent 0) 1)
;        ((=number? exponent 1) base)
;        ((=number? base 1) 1)
;        (else (list '** base exponent))))
;(put 'deriv '** exponentiation-deriv)

; 4. the only thing to change is to change
; the order of arguments in the procedure 'put

; ex 2.74
