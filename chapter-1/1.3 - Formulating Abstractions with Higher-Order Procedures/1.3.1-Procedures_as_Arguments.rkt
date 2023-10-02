#lang simply-scheme

(define (cube n) (* n n n))
(define (square n) (* n n))

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
;(define (simpson-rule f a b n)
;  (define h (/ (- b a) n))
 






















