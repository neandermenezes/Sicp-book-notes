#lang simply-scheme
;;helper functions
(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define dx 0.00001)

(require "./1.3.3-Procedures_as_General_Methods.rkt")

;;1.3.4 - Procedures as Returned Values
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x (square y))))
   1.0))


;;Newton’s method
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt-newton x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))


;;Abstractions and first-class procedures
(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-transform x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   1.0))

(define (sqrt-newton-transform x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

























