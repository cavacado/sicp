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

; ex 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
    (iter a 0))

;(define (integral-iter f a b dx)
;  (define (add-dx x) (+ x dx))
;  (* (sum-iter f a add-dx b) dx))

; ex 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact n)
  (product identity 1 inc n))

(define (double x)
  (* x 2))

(define (approx-pi n)
  (define (numerator-term x)
    (cond ((odd? x) (+ (- x 1) 2))
          ((even? x) (+ x 2))))
  (define (denominator-term x)
    (cond ((odd? x) (+ (- x 1) 3))
          ((even? x) (+ x 3))))
  (* 4.0
     (/ (/ (product numerator-term 0 inc n) 2)
        (product denominator-term 0 inc (- n 1)))))

; ex 1.31 pt 2

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (fact-iter n)
  (product-iter identity 1 inc n))

; ex 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-acc-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-acc-iter term a next b)
  (accumulate-iter * 1 term a next b))

; ex 1.33

(define (filter-acc combiner null-value predicate term a next b)
  (if (> a b)
      null-value
      (combiner (term (if (predicate a)
                          a
                          null-value))
                (filter-acc combiner null-value predicate term (next a) next b))))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n
                            (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next x)
  (cond ((= x 2) 3)
        (else (+ x 2))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-square-prime a b)
  (filter-acc + 0 prime? square a inc b))

; skip part 2
