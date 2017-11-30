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
;                 (make-exponentiation (base expr)
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

; quick answers because uncomfortable to write code w/o
; any means to test it

; 1.
; get-record should just return the '(addressx salaryx)

; 2.
; get-salary should be structured cadr of get-record
; get-record should return the contents '(addressx salaryx)
; cadr would then return the 2nd element, which is the salary required.

; 3.
; each individual division's files should be tagged with a label
; eg '('div1 namex '(adressx salaryx))
; find-employee-record then should expect a tag and a name to be able
; to retrieve the record

; 4.
; company needs to install record and salary generic procedures into
; the lookup table

;(define (make-from-real-imag-column x y)
;  (define (dispatch op)
;    (cond ((eq? op 'real-part) x)
;          ((eq? op 'imag-part) y)
;          ((eq? op 'magnitude)
;           (sqrt (+ (square x) (square y))))
;          ((eq? op 'angle) (atan y x))
;          (else
;           (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
;  dispatch)

; ex 2.75

;(define (make-from-mag-ang-column x y)
;  (define (dispatch op)
;    (cond ((eq? op 'magnitude) x)
;          ((eq? op 'angle) y)
;          ((eq? op 'real-part)
;           (* x (cos y)))
;          ((eq? op 'imag-part)
;           (* x (sin y)))
;          (else
;           (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
;  dispatch)

; ex 2.76
; for new operations, data-directed programming should be more appropriate
; for new types, message-passing programming should be more
; appropriate

(define (make-sum addend augend)
  (list '+ addend augend))

(define (sum-exp? e)
  (and (pair? e) (eq? (car e) '+)))

(define (sum-addend sum) (cadr sum))

(define (sum-augend sum) (caddr sum))

(define (eval-1 exp)
  (cond ((number? exp) exp)
        ((sum-exp? exp) (+ (eval-1 (sum-addend exp))
                           (eval-1 (sum-augend exp))))
        (else (error "unknown expression " exp))))

(define (make-range-2 min max)
  (list min max))

(define range-min-2 car)
(define range-max-2 cadr)

(define (range-add-2 r1 r2)
  (make-range-2 (+ (range-min-2 r1)
                   (range-min-2 r2))
                (+ (range-max-2 r1)
                   (range-max-2 r2))))

; (define (eval-2 exp)
;   (cond ((number? exp) exp)
;         ((sum-exp? exp)
;           (let ((v1 (eval-2 (sum-addend exp)))
;                 (v2 (eval-2 (sum-augend exp))))
;             (if (and (number? v1) (number? v2))
;                 (+ v1 v2)
;                 (range-add-2 v1 v2))))
;         ((pair? exp) exp)
;         (else (error "unknown exp " exp))))

(define constant-tag 'const)

(define (make-constant val)
  (list constant-tag val))

(define (constant-exp? e)
  (and (pair? e) (eq? (car e) constant-tag)))

(define constant-val cadr)

(define (constant-add exp1 exp2)
  (make-constant
    (+ (constant-val exp1) (constant-val exp2))))

(define (eval-3 exp)
  (cond ((constant-exp? exp) (constant-val exp))
        ((sum-exp? exp)
          (+ (eval-3 (sum-addend exp))
             (eval-3 (sum-augend exp))))
        (else (error "unknown expr type: " exp))))

(define (eval-4 exp)
  (cond ((constant-exp? exp) exp)
        ((sum-exp? exp)
          (constant-add (eval-4 (sum-addend exp))
                        (eval-4 (sum-augend exp))))
        (else (error "unknown expr type: " exp))))

(define range-tag 'range)

(define (make-range min max)
  (list range-tag min max))

(define (range-exp? e)
  (and (pair? e) (eq? (car e) range-tag)))

(define range-min cadr)
(define range-max caddr)

(define (range-add r1 r2)
  (make-range (+ (range-min r1)
                 (range-min r2))
              (+ (range-max r1)
                 (range-max r2))))

(define (val2range val)
  (make-range 0 val))

(define (eval-5 exp)
  (cond ((constant-exp? exp) exp)
        ((range-exp? exp) exp)
        ((sum-exp? exp)
         (let ((v1 (eval-5 (sum-addend exp)))
               (v2 (eval-5 (sum-augend exp))))
          (if (and (constant-exp? v1)
                   (constant-exp? v2))
              (constant-add v1 v2)
              (range-add (val2range v1)
                         (val2range v2)))))
        (else (error "unknown expr type: " exp))))

(define (value-exp? v)
  (or (constant-exp? v) (range-exp? v)))

(define (value-add-6 v1 v2)
  (if (and (constant-exp? v1) (constant-exp? v2))
           (constant-add v1 v2)
           (range-add (val2range v1)
                      (val2range v2))))

(define (eval-6 exp)
  (cond ((value-exp? exp) exp)
        ((sum-exp? exp)
         (value-add-6 (eval-6 (sum-addend exp))
                      (eval-6 (sum-augend exp))))
        (else (error "unknown expr type: " exp))))

(define limited-tag 'limited)

(define (limited-exp? e)
  (eq? 'limited (car e)))

(define (make-limited-precision val err)
  (list limited-tag val err))

(define (eval-7 exp)
  (cond ((value-exp? exp) exp)
        ((limited-exp? exp) exp)
        ((sum-exp? exp)
         (value-add-6 (eval-7 (sum-addend exp))
                      (eval-7 (sum-augend exp))))
        (else (error "unknown expr type: " exp))))

(define (value-add-7 v1 v2)
  (cond ((and (constant-exp? v1) (constant-exp? v2))
          (constant-add v1 v2))
        ((and (value-exp? v1) (value-exp? v2))
          (range-add (val2range v1)
                     (val2range v2)))
        (else
          (error "unknown exp: " v1 " or" v2))))

;; lecture 10 abstraction and tables

(define (find-assoc key alist)
  (cond ((null? alist) #f)
        ((equal? key (caar alist)) (cadar alist))
        (else (find-assoc key (cdr alist)))))

(define a1 '((x 15) (y 20)))

(define (add-assoc key val alist)
  (cons (list key val) alist))

(define a2 (add-assoc 'y 10 a1))

(define table1-tag 'table1)

(define make-table1 (cons table1-tag nil))

(define (table1-get tbl key)
  (find-assoc key (cdr tbl)))

(define (table1-put! tbl key val)
  (set-cdr! tbl (add-assoc key val (cdr tbl))))

(define t2-tag 'table2)

(define (make-table2 size hashfunc)
  (let ((buckets (make-vector size nil)))
     (list t2-tag size hashfunc buckets)))

(define (size-of tbl) (cadr tbl))

(define (hashfunc-of tbl) (caddr tbl))

(define (buckets-of tbl) (cadddr tbl))

(define (table2-get tbl key)
  (let ((index
        ((hashfunc-of tbl) key (size-of tbl))))
    (find-assoc key
                (vector-ref (buckets-of tbl) index))))

(define (table2-put! tbl key val)
  (let ((index
          ((hashfunc-of tbl) key (size-of tbl)))
        (buckets (buckets-of tbl)))
        (vector-set! buckets index
                     (add-assoc key val (vector-ref buckets
                                                    index)))))
