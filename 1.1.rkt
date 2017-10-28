#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y) 
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
      
(define (abso x)
  (cond ((< x 0) (- x))
        (else x)))

(define (absol x)
  (if (< x 0)
      (- x)
      x))

(define (range5-10 x)
  (and (> x 5) (< x 10)))
      
(define (>= x y)
  (or (> x y) (= x y)))

; ex 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; ex 1.3
(define (smallest-of-3 x y z)
  (cond ((and (< x y) (< x z)) x)
        ((and (< y x) (< y z)) y)
        ((and (< z x) (< z y)) z)))

(define (inter a x y z)
  (cond ((= a x) (sum-of-squares y z))
        ((= a y) (sum-of-squares x z))
        ((= a z) (sum-of-squares x y))))

(define (res x y z)
  (inter (smallest-of-3 x y z) x y z))

; ex 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; means that if b is positive, then the expression evaluates to a plus
;; else the expression will evaluate to a minus

; ex 1.5
; in the case of applicative-order evaluation, the interpreter will try to evaluate (p) first, [before the if statement] but since (p) is self-recursive, it will never terminate and thus the program would hang.
; but if its an normal-order evaluation, the interpreter would only evaluate (p) after the if statement and hence the statement is valid since the condition y would not trigger

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (define (new-if predicate
;                 then-clause
;                 else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))

; (define (new-sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess)
;           (new-sqrt-iter (improve guess x) x))

; (define (new-sqrt x)
;   (new-sqrt-iter 1.0 x))

; ex 1.6
; the program hangs,
; new-if is a procedure, it does not use normal-order evaluation, hence will produce a recursive procedure which loops forever
; if on the other hand is a special form as it uses applicative-order evaluation, will eagerly evaluate the first condition
; new-if evaluates both conditions whereas if only evaluates the first condition
; quote from solutions :
; 'This is key difference from new-if -- only one of the two consequent expressions get evaluated when using if, while both of the consequent expressions get evaluated with new-if.'

; ex 1.7
; for small numbers the tolerance 0.001 is not enough.
; for very large numbers the machine might not be able to note the difference between the 2 numbers, and since the condition will never be fufilled, it would not terminate

(define (good-enough'? guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.001)))
   
(define (sqrt-iter' guess x)
  (if (good-enough'? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt' x)
  (sqrt-iter' 1.0 x))

; If good-enough'? uses the alternative strategy (a relative tolerance of 0.001 times the difference between one guess and the next), sqrt works better both for small and large numbers.

; ex 1.8

(define (cbrt-iter guess x)
  (if (good-enough-cb? guess x)
      guess
      (cbrt-iter (improve-cb guess x) x)))

(define (improve-cb guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube x)
  (* x x x))

(define (good-enough-cb? guess x)
  (< (abs (- (cube guess) x)) 0.001))
 
(define (cbrt x)
  (cbrt-iter 1.0 x))

 