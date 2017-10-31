#lang sicp

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

;(define (linear-combination-new a b x y)
;  (add (mul a x) (mul b y)))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y
           (remainder x y))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;(define (make-rat n d)
;  (let ((g (gcd n d)))
;    (cons (/ n g)
;          (/ d g))))

; ex 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (> n 0) (< d 0)) (cons (* -1 (/ n g))
                                       (* -1 (/ d g))))
          (else (cons (/ n g)
                      (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat
 (add-rat one-half one-third))
(print-rat
 (mul-rat one-half one-third))
(print-rat
 (add-rat one-third one-third))

(define minus-two-five (make-rat -2 5))
(print-rat minus-two-five)

(define two-five (make-rat -2 -5))
(print-rat two-five)

(define minus-two-eight (make-rat 2 -8))
(print-rat minus-two-eight)

; ex 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment x)
  (car x))

(define (end-segment x)
  (cdr x))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point
   (/ (+ (x-point (start-segment segment))
         (x-point (end-segment segment))) 2)
   (/ (+ (y-point (start-segment segment))
         (x-point (end-segment segment))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define cus-segment
  (make-segment (make-point 0 0)
                (make-point 10 10)))

; ex 2.3
