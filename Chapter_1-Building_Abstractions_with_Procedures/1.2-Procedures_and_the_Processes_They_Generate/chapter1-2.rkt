#lang simply-scheme
;counting different ways to give change to an amount
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)) )))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;exercise1.11
(define (math-recursive n)
  (if (< n 3)
      n
      (+ (math-recursive (- n 1)) (* 2 (math-recursive (- n 2))) (* 3 (math-recursive (- n 3)))) ))


(define (math-iterative n)
  (define (math-helper n1 n2 n3 count)
    (if (= count 2)
        n1
        (math-helper (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (- count 1))))
  
  (if (< n 3)
      n
      (math-helper 2 1 0 n)))

;exercise1.12
(define (pascal x y)
  (cond ((or (= y 0) (= y x)) 1) ; first and last elements of each row are 1
        (else (+ (pascal (- x 1) (- y 1)) ; sum of the two elements above in the previous row
                (pascal (- x 1) y)))))


;;recursive linear expt
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;;iterative linear expt
(define (helper b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter
       b
       (- counter 1)
       (* b product))
      ))

;;logarithmic recursive exponentiation
(define (square n) (* n n))

(define (fast-expt b n)
  (cond ((= n 0)
        1)
  ((even? n)
   (square (fast-expt b (/ n 2))))
  (else
   (* b (fast-expt b (- n 1))))))

;;exercise 1.16
;;logarithmic iterative exponentiation
(define (fast-expt-iter base n)
  (define (impl base n acc)
     (cond ((= n 0)
        acc)
       ((even? n) (impl (square base) (/ n 2) acc))
       (else (impl base (- n 1) (* acc base)))))
  (impl base n 1))

;;exercise 1.17
;;linear multiplication using additions
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double n) (+ n n))
(define (halve n) (/ n 2))

;;log multiplication recursive
(define (fast-multiplication a b)
  (cond ((= b 0)
         0)
         ((even? b)
          (double (fast-multiplication a (halve b))))
         (else(+ a (fast-multiplication a (- b 1))))))

;;exercise 1.18
(define (fast-multiplication-iter a b)
  (define (fast-mult a b acc)
    (cond ((= b 0)
           acc)
          ((even? b) (fast-mult (double a) (halve b) acc))
          (else (fast-mult a (- b 1) (+ acc a)))))
  (fast-mult a b 0))

;;exercise 1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) 
           b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q p))  ; Compute p'
                     (+ (* 2 p q) (* q q))  ; Compute q'
                     (/ count 2)))
          (else 
           (fib-iter (+ (* b q) 
                        (* a q) 
                        (* a p))
                     (+ (* b p) 
                        (* a q))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))

;;GREATEST COMMON DIVISORS
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;DIVISORS AND PRIME NUMBERS
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(provide prime?)
;;fermat prime theorem

;; is 5 prime?, pick 3 for testing
;; fermat = a^p / p = a
;; a^p = 243, 243 % 5 = 3
;; so 5 is prime, because when we pick an arbitrary number as a base to a 5exp, the result of it MOD the prime number is itself.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
         (else (remainder (* base (expmod base (- exp 1) m)) m))))

;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #T)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else #F)))

;;exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;;exercise 1.22
(define (runtime) (current-inexact-milliseconds)) ; adapting to DrRacket

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start-range end-range)
  (if (even? start-range) (search-for-primes (+ start-range 1) end-range)
        (cond ((> start-range end-range) (newline) (display "done"))
        (else (timed-prime-test start-range) (search-for-primes (+ 2 start-range) end-range)))))

;(search-for-primes 1000 1020)
;(search-for-primes 10000 10020)
;(search-for-primes 100000 100020)
;(search-for-primes 1000000 1000020)

;  p      prime?  next   fast-prime?
;;1009 - 0.083    halve   depends on times variable, but roughly 1.44
;;1013 - 0.083
;;1019 - 0.084


;;10007 - 2.554
;;10009 - 2.375
;;10039 - 2.549

;;100019 - 8.203
;;100043 - 9.785
;;100049 - 8.559

;;1000003 - 81.933 halve  extremely bad running time, roughly 4288 ms
;;1000033 - 81.544
;;1000037 - 81.140

;;EXERCISE 1.23
; after implementing "next" procedure, the running time averagely halved for all above conditions

;;EXERCISE 1.27
(define (carmichael? n)
  (define (carmichael-helper n a)
    (cond ((= a 1) #t)
          ((not (= (expmod a n n) a)) #f)
          (else (carmichael-helper n (- a 1)))
    ))
  (carmichael-helper n (- n 1)))

;;EXERCISE 1.28 TODO




























