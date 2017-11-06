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