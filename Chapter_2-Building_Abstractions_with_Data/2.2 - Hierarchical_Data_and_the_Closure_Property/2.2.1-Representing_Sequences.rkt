#lang simply-scheme

;;2.2.1 - Representing Sequences

(define one-through-four (list 1 2 3 4))

;;This works like a list in js with indexes of n = 0 up to length - 1, let list = [1, 2, 3, 4].
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
;(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-iter items count)
  (if (null? items)
      count
      (length-iter (cdr items) (+ 1 count))))

(define odds (list 1 3 5 7 9))
;(length odds)
;(length-iter odds 0)


;;EXERCISES
;;2.17 - receives a list and returns last element
;Here we use the helper length procedure to return the wanted index.
(define (last-pair list)
  (let ((size (length list)))
    (define (iter list counter)
      (if (= counter size)
          (car list)
          (iter (cdr list) (+ 1 counter))))
    (iter list 1)))

(display "2.17 result: ")
(last-pair (list 1 2 50)); = 50
(newline)

;;2.18 - reverse list
(define (reverse list)
  (define (iter list result)
    (if (null? list)
        result
        (iter (cdr list) (cons (car list) result))))
  (iter list '()))

;(cons (car item) (recurse but-last-list)

(display "2.18 result: ")
(define to-reverse (list 1 2 3 4 5))
(reverse to-reverse)
(newline)

;;2.19 - coin change
;;In this exercise we refactor our coin-values algorithm to accept a list of coins as argument.
;;The order of the coin list is irrelevant, the algorithm still holds because of
;;the abstraction it uses.
(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(define reverse-us-coins (list 25 50 10 5 1))

(display "2.19 result in normal order: ")
(cc 100 us-coins)
(newline)

(display "2.19 result in reverse order: ")
(cc 100 reverse-us-coins)
(newline)


;;2.20 - same parity
;;probably not the cleanest solution. We check the oddity of the first integer and
;;store it as a boolean, for each subsequent integer we compare wheter it is even or odd
;;and compare it to the previously stored is-even? variable before adding it to the list.
(define (same-parity . integers)
  (let ((is-even? (even? (car integers))))
  (define (iter list)
    (if (null? list)
        list
        (if (equal? (even? (car list)) is-even?)
            (cons (car list) (iter (cdr list)))
            (iter (cdr list)))))
  (iter integers)))

(display "2.20 result: ")
(same-parity 2 1 2 3 4 5 6)
(newline)


;;2.21 - square list
;;pretty straightforward, we use the map function to iterate each element to its square.
(define (square n) (* n n))
(define (square-list list)
  (map square list))

(display "2.21 result: ")
(square-list (list 1 2 3 4))


;;2.22 - iter square list bug
;;I had this bug on the same-parity exercise

;;in this algorithm everytime we add an element, we are adding to the start of the list
;;so appending (square 1) (result) = (1)
;;             (square 2) (result) = (4, 1). And so on
(define (square-list-iter-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))
;result = '(16 9 4 1)
;(square-list-iter-1 (list 1 2 3 4))

;;In trying to bypass this bug by using cons (result - square) we are creating
;;multiple pairs where car is the previous pair value.
(define (square-list-iter-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items '()))

;result = '((((() . 1) . 4) . 9) . 16)
;(square-list-iter-2 (list 1 2 3 4))


;;2.23 - for each implementation
(define (for-each-impl procedure list)
  (define (iter elements)
    (cond ((null? elements) (display "end of list"))
          (else
           (display (procedure (car elements)))
           (newline)
           (iter (cdr elements)))))
  (iter list))

(for-each-impl square (list 1 2 3 4 5 6))












