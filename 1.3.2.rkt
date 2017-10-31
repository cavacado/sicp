#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
       
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))
 
(define (cube x)
  (* x x x))
     
(define (integral f a b dx)
  (* (sum f 
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
      dx))

(define (square x)
  (* x x))

; (define (f x y)
;   (define (f-helper a b)
;     (+ (* x (square a))
;       (* y b)
;       (* a b)))
;   (f-helper (+ 1 (* x y))
;             (- 1 y)))

; (define (f x y)
;   ((lambda (a b)
;     (+ (* x (square a))
;         (* y b)
;         (* a b)))
;   (+ 1 (* x y))
;   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
     
; ex 1.34
; (define (f g) (g 2))
; if we ask the interpreter to evalutae (f f)
; it will evaluate => (f 2)
; then (f 2) will throw an error since 2 isnt a fn

; Note that both substitution models, applicative-order evaluation 
; and normal-order evaluation, will lead to the same expansion.

(define (average x y)
  (/ (+ x y) 2.0))

(define (search f neg-point pos-point)
  (let ((midpoint
          (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
           (cond
             ((positive? test-value)
              (search f neg-point midpoint))
             ((negative? test-value)
              (search f midpoint pos-point))
             (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
       (if (close-enough? guess next)
           next
           (try next))))
  (try first-guess))
           
(define (sqrt-cus x)
  (fixed-point (lambda (y) (average y (/ x y))
               1.0)))

; ex 1.35

;(define golden-ratio
;  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; ex 1.36

(define (x-power-x y)
  (fixed-point (lambda (x) (/ (log y) (log x))) 1.1))

(define (x-power-x-avg-damp y)
  (fixed-point (lambda (x) (average x (/ (log y) (log x)))) 1.1))

; ex 1.37
(define (cont-frac n d k)
  (define (cont-frac-iter n d count k)
    (if (= count k)
        (/ (n k) (d k))
        (/ (n count)
           (+ (d count)
              (cont-frac-iter n d (inc count) k)))))
  (cont-frac-iter n d 1 k))
      
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           11)

; phi => 0.6180469715698392
; roughly around 11

; ex 1.37 pt 2
(define (cont-frac-recurse n d k)
  (if (= k 0)
      0
      (/ (n 1)
         (+ (d 1)
            (cont-frac-recurse (lambda (x) (n (+ x 1)))
                               (lambda (x) (d (+ x 1)))
                               (- k 1))))))

; ex 1.38
(define (approx-e k)
  (define (d-eqn x)
    (cond ((= x 2) 2)
          ((= (remainder (- x 2) 3) 0)
           (* 2 (+ (/ (- x 2) 3) 1)))
          (else 1)))
  (+ 2.0 (cont-frac (lambda (i) 1.0)
             d-eqn
             k)))

; ex 1.39
(define (tan-cf x k)
  (define (d-eqn i)
    (if (= i 1)
        1
        (+ 2 (d-eqn (- i 1)))))
  (define (tan-cf-iter x count k)
    (cond ((= count k) (/ (square x) (d-eqn k)))
          ((= count 1) (/ x (- 1
                               (tan-cf-iter x (inc count) k))))
          (else (/ (square x)
                   (- (d-eqn count)
                      (tan-cf-iter x (inc count) k))))))
  (tan-cf-iter x 1 k))

; after ~ 4 terms then it will converge to the value of tan x

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (sqrt-cus-new x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
   1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-xform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-xform g)
               guess))

(define (sqrt-newton x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fixed-one x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt-fixed-two x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-xform
   1.0))

; ex 1.40

(define (cubic a b c)
  (lambda (y) (- (cube y) (* a (square y)) (* b y) c)))

(define (cubic-of x a b c)
  (newtons-method
   (cubic a b c)
   1.0))

; ex 1.41
(define (double-cus f)
  (lambda (x)
    (f (f x))))

; (((double-cus (double-cus double-cus)) inc) 5) => 21

; ex 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; ex 1.43
;(define (repeated f n)
;  (lambda (x)
;    (if (= n 1)
;        (f x)
;        ((repeated (compose f f) (- n 1)) x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

; ex 1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
       (f x)
       (f (+ x dx)))
       3)))

(define (n-fold f n)
  ((repeated smooth n) f))

; ex 1.45
(define (n-root x n)
    (fixed-point
     ((repeated
       average-damp
       n)
      (lambda (y)
         (/ x (expt y (- n 1)))))
     1.0))

; ex 1.46...
(define (iterative-improve f g)
  (lambda (guess)
    (let ((next (g guess)))
      (if (f guess next)
          next
          ((iterative-improve f g) next)))))

(define (close-enough-iterative? x y)
  (< (abs (- x y)) tolerance))

(define (sqrt-iterative-improve x)
  ((iterative-improve close-enough-iterative?
                     (average-damp
                      (lambda (y) (/ x y)))) 1.0))

(define (fixed-point-iterative-improve f first-guess)
  ((iterative-improve close-enough-iterative?
                      f) first-guess))

(define (test-iterative-fix x)
  (fixed-point-iterative-improve (average-damp
                                  (lambda (y) (/ x y)))
                                 1.0))
