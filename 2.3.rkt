#lang sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; ex 2.53
; (list 'a 'b 'c) => (a b c)
; (list (list 'george)) => ((george))
; (cdr '((x1 x2) (y1 y2))) => ((y1 y2))
; (cadr '((x1 x2) (y1 y2))) => (y1 y2)
; (pair? (car '(a short list))) => #f
; (memq 'red '((red shoes) (blue socks))) => #f
; (memq 'red '(red shoes blue socks)) => #t

; ex 2.54

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))
        ((and (list? (car l1)) (list? (car l2))) (and (equal? (car l1) (car l2))
                                                      (equal? (cdr l1) (cdr l2))))
        (else #f)))

; ex 2.55

; (car ''abracadabra) => the symbol is (quote abracadabra) so the car is 'quote

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp) (make-product (make-product (exponent exp)
                                                           (make-exponentiation (base exp) (- (exponent exp) 1)))
                                             (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (cond ((pair? (cdddr s)) (make-sum (caddr s)
                                     (augend (cons '+ (cons 'x (cdddr s))))))
        (else (caddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (cond ((pair? (cdddr p)) (make-product (caddr p)
                                         (multiplicand (cons '* (cons 'x (cdddr p))))))
        (else (caddr p))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

; ex 2.57
; ex 2.58

(define (deriv-infix exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum-infix? exp) (make-sum-infix (deriv-infix (addend-infix exp) var)
                                          (deriv-infix (augend-infix exp) var)))
        ((product-infix? exp) (make-sum-infix (make-product-infix (multiplier-infix exp)
                                                            (deriv-infix (multiplicand-infix exp) var))
                                        (make-product-infix (deriv-infix (multiplier-infix exp) var)
                                                            (multiplicand-infix exp))))
        (else (error "unknown expression type: DERIV" exp))))

(define (make-sum-infix a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product-infix m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (sum-infix? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend-infix s)
  (car s))

(define (augend-infix s)
  (cond ((and (sum-infix? (caddr s)) (pair? (caddr s)))
         (make-sum-infix (car (caddr s))
                         (augend-infix (caddr s))))
        ((and (product-infix? (caddr s)) (pair? (caddr s)))
         (make-product-infix (car (caddr s))
                             (multiplicand-infix (caddr s))))
        (else (caddr s))))

(define (product-infix? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier-infix p)
  (car p))

(define (multiplicand-infix p)
  (cond ((and (product-infix? (caddr p)) (pair? (caddr p)))
         (make-product-infix (car (caddr p))
                             (multiplicand-infix (caddr p))))
        ((and (sum-infix? (caddr p)) (pair? (caddr p)))
         (make-sum-infix (car (caddr p))
                         (augend-infix (caddr p))))
        (else (caddr p))))

; ex 2.58 pt2

(define (preprocess exp)
  (cond ((and (= (length exp) 3)
              (pair? (caddr exp)))
         (list (car exp)
               (cadr exp)
               (preprocess (caddr exp))))
        ((= (length exp) 3) exp)
        (else (list (car exp)
                    (cadr exp)
                    (preprocess (cdr (memq (cadr exp) exp)))))))
