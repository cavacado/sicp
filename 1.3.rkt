#lang sicp

(define (cube x)
  (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes-new a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers-new a b)
  (sum identity a inc b))

(define (pi-sum-new a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; ex 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (eqn x)
    (f (+ a (* x h))))
  (define (simpson-term x)
    (cond ((or (= x 0) (= x n))
           (eqn x))
          ((even? x)
           (* 4 (eqn x)))
          ((odd? x)
           (* 2 (eqn x)))))
  ( * (/ h 3.0) (sum simpson-term 0 inc n)))

;(simpson cube 0 1 100) => 0.24671666666666658
;(simpson cube 0 1 1000) => 0.249667166666667