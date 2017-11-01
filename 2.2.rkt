#lang sicp

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define squares
  (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds
  (list 1 3 5 7))

(define (length-iterative items)
  (define (length-iter x count)
    (if (null? x)
        count
        (length-iter (cdr x) (+ 1 count))))
  (length-iter items 0))

(define (append-new l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append-new (cdr l1) l2))))

; ex 2.17
(define (last-pair list)
  (if (null? (cdr list))
      (cons (car list) nil)
      (last-pair (cdr list))))

; ex 2.18
(define (reverse-new list)
  (if (null? (cdr list))
      list
      (append (reverse-new (cdr list))
              (cons (car list) nil))))

;(reverse-new (list 1 2 3))
;(append (reverse-new (list 2 3)) (list 1))
;(append (append (reverse-new (list 3)) (list 2)) (list 1))
;(append (append (list 3)  (list 2)) (list 1))
;(append (list 3 2) (list 1))
;(list 3 2 1)

; ex 2.19

(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)

(define except-first-denomination cdr)

(define no-more? null?)

; the order of the list does not matter
; the value will cancel out eventually
; since it computes all possible combintations

; ex 2.20

(define (same-parity i . l)
  (define (helper list predicate)
    (cond ((null? list)
           nil)
          ((predicate (car list))
           (cons (car list) (helper (cdr list) predicate)))
          (else (helper (cdr list) predicate))))
  (if (odd? i)
      (cons i (helper l odd?))
      (cons i (helper l even?))))
