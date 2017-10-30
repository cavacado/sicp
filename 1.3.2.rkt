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
