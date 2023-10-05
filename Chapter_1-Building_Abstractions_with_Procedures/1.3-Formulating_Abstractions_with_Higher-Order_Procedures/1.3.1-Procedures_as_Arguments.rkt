#lang simply-scheme

(define (cube n) (* n n n))
(define (square n) (* n n))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-integers-func a b)
  (if (> a b)
      0
      (+ a (sum-integers-func (+ a 1) b))))

(define (sum-cubes-func a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes-func (+ a 1) b))))

(define (pi-sum-func a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum-func (+ a 4) b))))

;;high-order function to express the concept of summation
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;;pi = (* 8 (pi-sum 1 5000))

;;integral of a function f between a and b
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))



;;EXERCISES
;;1.29
;h = (upper range - lower range) / number of rectangles
;h/3(y0+4y1+2y2+4y3+2y4+⋯+2yn-2+4yn-1+yn)
;we can express it as two summations, odd y values are multiplied by 4, even are multiplied by 2
;y0 and yn is always multiplied by 1
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x h h))
  (* (+ (f a) ;y0
        (* 4 (sum f (+ a h) add-2h b)) ;starts at y2
        (* 2 (sum f a add-2h b)) ;starts at y1
        (f b)) ;yn
     (/ h 3)))

;(integral cube 0 1.0 0.0001) ;0.24999999874993412

;(simpson-integral cube 0 1.0 100) ;0.25000000000000044
;(simpson-integral cube 0 1.0 1000) ;0.25000000000000083

;;1.30
;simply in each call to iter we pass next(a) and result + f(a)
(define (sum-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum cube 1 inc 5)
(sum-iterative cube 1 inc 5)

;;1.31
;defining a procedure that computes product, analogous to summation procedures
(define (product f a next b)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) next b))))

;defining a factorial in terms of product procedure
;(product identity 1 inc 5); result = 120

;π4=2⋅4⋅4⋅6⋅6⋅8⋅⋯3⋅3⋅5⋅5⋅7⋅7⋅⋯

;wallis product is the following function: (2n/2n - 1)*(2n/2n + 1) iteratively multiplied by increments of 1
;until upper bound is reached. The bigger the upper range the nearer the result will be pi/2

;argument n is the number of wanted multiplications, the bigger n is, the more accurate pi/4 will be.
(define (pi-aprox-inc x) (+ x 2))
(define (wallis-formula n)
  (* (/ (* 2 n)
        (- (* 2 n) 1))
     (/ (* 2 n)
        (+ (* 2 n) 1))))

;n is the number of iterations
(define (wallis-product n)
  (* 2.0 (product wallis-formula 1.0 inc n)))

(wallis-product 1000);3.140807746030383

;iterative product
(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;1.32
;generalizing sum and product procedures one step further
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;accumulate procedure iterative form
(define (accumulate-iterative combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(accumulate + 0 identity 1 inc 5); 15
(accumulate * 1 identity 1 inc 5); 120

(accumulate-iterative + 0 identity 1 inc 5); 15
(accumulate-iterative * 1 identity 1 inc 5); 120
      
;1.33
;in order to add a filter on which numbers to accumulate we simply nest an if condition that tests if the value
;attends a predicate.
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                (filtered-accumulate combiner null-value term (next a) next b filter))
          (filtered-accumulate combiner null-value term (next a) next b filter)
          )))

;here we add the prime numbers filter to accumulate the square of those that are prime
(filtered-accumulate + 0 square 1 inc 5 prime?); 39

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;product of all primes where GCD(i, n) = 1
(define (product-of-relative-prime n)
  (define (gcd1 i)
    (= 1 (gcd i n)))
  (filtered-accumulate * 1 identity 1 inc n gcd1))























