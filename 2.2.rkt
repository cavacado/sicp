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

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list-new items factor)
  (map (lambda (x) (* x factor))
       items))

; ex 2.21

(define (square x)
  (* x x))

(define (square-list-low items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-low (cdr items)))))

(define (square-list-high items)
  (map square items))

; ex 2.22
(define (square-list-iterative items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;(square-list-iterative (list 1 2 3 4))
;(iter (list 1 2 3 4) nil)
;(iter (list 2 3 4) (cons 1 nil))
;(iter (list 3 4) (cons 4 (cons 1 nil)))
;(iter (list 4) (cons 9 (cons 4 (cons 1 nil))))
;(iter nil (cons 16 (cons 9 (cons 4 (cons 1 nil)))))
;=> (list 16 9 4 1)
; order of cons is reversed
;(iter (list 1 2 3 4) nil)
;(iter (list 2 3 4) (cons nil 1))
;(iter (list 3 4) (cons (cons nil 1) 4))
;(iter (list 4) (cons (cons (cons nil 1) 4) 9))
;(iter nil (cons (cons (cons (cons nil 1) 4) 9) 16))
;(cons (cons (cons (cons nil 1) 4 9 16)))
; swapping cons around doesnt change the fact that cons
; constructs from the front, the resulting result is not a list

; ex 2.23

(define (for-each proc list)
  (cond ((not (null? list))
         (proc (car list))
         (for-each proc (cdr list)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

; ex 2.24

;(list 1 (list 2 (list 3 4)))
;(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil))
;
;|---|---|
;| 1 | | |
;|---|-|-|
;    |-|-|---|
;    | 2 | | |
;    |---|-|-|
;        |-|-|---|
;        | 3 | 4 |
;        |---|---|

;       (1 (2 (3 4)))
;      /         \
;      1       (2 (3 4)
;                /   \
;                2    (3 4)
;                      /  \
;                      3   4

; ex 2.25

;(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
;(car (car (list (list 7))))
;(car
; (cdr
;  (car
;   (cdr
;    (car
;     (cdr
;      (car
;       (cdr
;        (car
;         (cdr
;          (car
;           (cdr
;            (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

; ex 2.26

(define z (list 1 2 3))
(define y (list 4 5 6))
;(append z y) => (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 nil))))))
;(cons z y) => (cons
;               (cons 1 (cons 2 (cons 3 nil)))
;               (cons 4 (cons 5 (cons 6 nil))))
;(list z y) => (cons
;               (cons 1 (cons 2 (cons 3 nil)))
;               (cons (cons 4 (cons 5 (cons 6 nil))) nil))

; ex 2.27

(define xx
  (list (list 1 2) (list 3 4)))

(define yy
  (list (list 3 4) (list 2 5)))

(define xxx
  (list xx yy))

(define (deep-reverse list)
  (if (pair? list)
      (cons (deep-reverse (car (reverse list))) (deep-reverse (cdr (reverse list))))
      list))

; ex 2.28

(define (fringe tree)
  (cond ((pair? tree) (append (fringe (car tree))
                              (fringe (cdr tree))))
        ((null? tree) nil)
        (else (list tree))))

; ex 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile) (+ (branch-length (car mobile))
                           (branch-length (cdr mobile))))
        (else 1)))

(define (branch-structure mobile)
  (cond ((null? mobile) nil)
        ((pair? mobile) (append (branch-structure (car mobile))
                                (branch-structure (cdr mobile))))
        (else (list mobile))))

(define (total-weight mobile)
  (define structure (branch-structure mobile))
  (define (worker struct)
    (if (null? struct)
        0
        (+ (car struct)
           (worker (cdr struct)))))
  (worker structure))

(define (balanced? mobile)
  (define (torque branch)
    (* (total-weight branch)
       (branch-length branch)))
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(define zz
  (list (list 3 5) (list 4 4)))

(define (make-mobile-new left right)
  (cons left right))

(define (make-branch-new length structure)
  (cons length structure))

; quite a fair bit since list a b /= cons a b

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree (car tree) factor)
               (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; ex 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
         tree))

; ex 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree-map-new tree)
  (tree-map square tree))

; ex 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

(define answer
  (list (list nil)
        (list 3)
        (list 2)
        (list 2 3)
        (list 1)
        (list 1 3)
        (list 1 2)
        (list 1 2 3)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (append (enumerate-tree (car tree))
                              (enumerate-tree (cdr tree))))
        (else (list tree))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd?
                           sequence))))

;(define (salary-of-highest-paid-programmer records)
;  (accumulate max
;              0
;              (map salary
;                   (filter programmer? records))))

; ex 2.33
(define (map-acc p sequence)
  (accumulate (lambda (x y)
                (cons (p x)
                      y))
              nil
              sequence))

(define (append-acc seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length-acc sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))

; ex 2.34

(define (horner-eval x coefficient-sequence)
  (/ (accumulate (lambda (this-coeff higher-terms)
                (* (+ this-coeff higher-terms) x))
              0
              coefficient-sequence) x))
