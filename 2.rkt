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

(define (make-rect a b c d)
  (cons a (cons b (cons c d))))

(define (get-a rect)
  (car rect))

(define (get-b rect)
  (car (cdr rect)))

(define (get-c rect)
  (car (cdr (cdr rect))))

(define (get-d rect)
  (cdr (cdr (cdr rect))))

(define (square x)
  (* x x))

(define cus-rect
  (make-rect  (make-point 0 0)
              (make-point 0 2)
              (make-point 2 2)
              (make-point 2 0)))

(define (distance-between c1 c2)
  (sqrt (+ (square (- (x-point c2) (x-point c1)))
           (square (- (y-point c2) (y-point c1))))))

(define (perimeter rect)
  (+ (distance-between (get-a rect) (get-b rect))
     (distance-between (get-b rect) (get-c rect))
     (distance-between (get-c rect) (get-d rect))
     (distance-between (get-d rect) (get-a rect))))

(define (area rect)
  (* (distance-between (get-a rect) (get-b rect))
     (distance-between (get-a rect) (get-d rect))))

(define (make-rect-2 l1 l2 l3 l4)
  (cons (start-segment l1)
        (cons (start-segment l2)
              (cons (start-segment l3)
                    (start-segment l4)))))

(define cus-rect-2
  (make-rect-2 (make-segment (make-point 0 0)
                             (make-point 10 10))
               (make-segment (make-point 0 2)
                             (make-point 0 8))
               (make-segment (make-point 2 2)
                             (make-point 2 9))
               (make-segment (make-point 2 0)
                             (make-point -10 9))))

(define (cons-cus x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car-cus z) (z 0))
(define (cdr-cus z) (z 1))

; ex 2.4

(define (cons-alt x y)
  (lambda (m) (m x y)))

(define (car-alt z)
  (z (lambda (p q) p)))

(define (cdr-alt z)
  (z (lambda (p q) q)))

; ex 2.5

(define (cons-positive x y)
  (* (expt 2 x) (expt 3 y)))

(define (car-positive z)
  (define (helper x count)
    (if (not (integer? (/ x 2)))
        count
        (helper (/ x 2) (inc count))))
  (helper z 0))

(define (cdr-positive z)
  (define (helper x count)
    (if (not (integer? (/ x 3)))
        count
        (helper (/ x 3) (inc count))))
  (helper z 0))

; ex 2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  ;(lambda (f) (lambda (x) (f (((lambda (a) (lambda (b) (a b))) f) x))))
  ;(lambda (f) (lambda (x) (f ((lambda (b) (f b)) x))))
  (lambda (f) (lambda (x) (f (f x)))))

(define (add-n a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; ex 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "cannot divide interval with span of 0")
      (mul-interval x
                    (make-interval
                     (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

; ex 2.8
; difference of the two intervals could be the difference of the two lower bounds
; as the lower bound and the difference of the two upper bounds
; could be the upper bound.

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (lower-bound y))
                 (- (upper-bound x)
                    (upper-bound y))))

; ex 2.9

(define (width lower upper)
  (* 0.5 lower upper))

;a => (make-interval x1 y1)
;b => (make-interval x2 y2)
;(add-interval a b)
;=> (make-interval (+ x1 x2)
;                  (+ y1 y2))
;=> (make-interval (f (x1+x2, y1+y2))
;=> (width (make-interval (x1+x2 y1+y2)))
;=> (* 0.5 (+ x1 x2) (+ y1 y2))
;=> f (x y) (* 0.5 (+ x y)) // shown

;(mul-interval a b)
;=> p1 => (* x1 x2)
;=> p2 => (* x1 y2)
;=> p3 => (* x2 y1)
;=> p4 => (* y1 y2)
;(make-interval ((min p1 p2 p3 p4) (max p1 p2 p3 p4)) /=> (f (?? ??))
; sub x1 = 1 x2 = 2 y1 = 2 y2 = 3
; p1 => 2 p2 => 3 p3 => 4 p4 => 6
;(make-interval 2 6)
;(width (make-interval 2 6))
; -> 4 /=> f(w1 w2) /=> false

; ex 2.11

;; copied because ben diddle is a dick
(define (mul-interval-new x y) 
   ;; endpoint-sign returns: 
   ;;     +1 if both endpoints non-negative, 
   ;;     -1 if both negative, 
   ;;      0 if opposite sign 
   (define (endpoint-sign i) 
     (cond ((and (>= (upper-bound i) 0) 
                 (>= (lower-bound i) 0)) 
            1) 
           ((and (< (upper-bound i) 0) 
                 (< (lower-bound i) 0)) 
            -1) 
           (else 0))) 
  
   (let ((es-x (endpoint-sign x)) 
         (es-y (endpoint-sign y)) 
         (x-up (upper-bound x)) 
         (x-lo (lower-bound x)) 
         (y-up (upper-bound y)) 
         (y-lo (lower-bound y))) 
  
     (cond ((> es-x 0) ;; both x endpoints are +ve or 0 
            (cond ((> es-y 0) 
                   (make-interval (* x-lo y-lo) (* x-up y-up))) 
                  ((< es-y 0) 
                   (make-interval (* x-up y-lo) (* x-lo y-up))) 
                  (else 
                   (make-interval (* x-up y-lo) (* x-up y-up))))) 
  
           ((< es-x 0) ;; both x endpoints are -ve 
            (cond ((> es-y 0) 
                   (make-interval (* x-lo y-up) (* x-up y-lo))) 
                  ((< es-y 0) 
                   (make-interval (* x-up y-up) (* x-lo y-lo))) 
                  (else 
                   (make-interval (* x-lo y-up) (* x-lo y-lo))))) 
  
           (else  ;; x spans 0 
            (cond ((> es-y 0) 
                   (make-interval (* x-lo y-up) (* x-up y-up))) 
                  ((< es-y 0) 
                   (make-interval (* x-up y-lo) (* x-lo y-lo))) 
                  (else 
                   ;; Both x and y span 0 ... need to check values 
                   (make-interval (min (* x-lo y-up) (* x-up y-lo)) 
                                  (max (* x-lo y-lo) (* x-up y-up)))))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width-new i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

; this is wrong though
;(define (make-center-percent c p)
;  (make-interval (- c (* p c)) (+ c (* p c))))
;
;(define (percent i)
;  (* (/ (- (upper-bound i)
;           (center i))
;        (center i)) 100))
;... come back another day i guess