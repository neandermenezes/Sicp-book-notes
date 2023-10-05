#lang simply-scheme

;;helper procedures
(define (cube n) (* n n n))
(define (square n) (* n n))
(require "./1.3.1-Procedures_as_Arguments.rkt")

;;1.3.2 Constructing Procedures Using Lambda and Let
(define (pi-sum-lambda a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a (lambda (x) (+ x 4)) b))

(define (integral-lambda f a b dx)
  (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b) dx))

;;forms of defining an internal procedure, variable
;;nested procedures
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a)) (* y b) (* a b)))
  (f-helper (+ 1 (* x y)) (- 1 y)))

;;lambda procedure
(define (f-lambda x y)
  ((lambda (a b) (+ (* x (square a)) (* y b) (* a b))) (+ 1 (* x y)) (- 1 y) ))

;;let variable
(define (f-let x y)
  (let ( (a (+ 1 (* x y))) (b (- 1 y)) )
    (+ (* x (square a)) (* y b) (* a b) ) ))