#lang simply-scheme
;;helper functions
(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))
(define (cube x) (* x x x))

(require "./1.3.3-Procedures_as_General_Methods.rkt")

;;1.3.4 - Procedures as Returned Values
;average-damp returns a procedure that is: average of x + f(x)
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

;for instance, when calling sqrt with value 4 the following stack will be produced
;(average-damp (λ (y) (/ 4 y)) returns a new function (/ (y + (/ 4 y)) 2)
;fixed-point will test 1.0 first guess which substitutes the y value
;(/ (1 + (/ 4 1)) 2) -> 2.5, eventually returning aproximately 2
;
;(define (sqrt x)
; (fixed-point 
;  (lambda (y) (average y (/ x y)))
;  1.0))
;
;what we did is delegate the averaging part to average-damp, so we need only to call the actual
;sqrt gradual aproximation x/y
(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

;cube root is x / y², again, to avoid infinite loops, we call average-damp
(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x (square y))))
   1.0))


;;Newton’s method
;fixed point of f(x) = x - [g(x)/Dg(x)], Dg(x) being the derivative of g evaluated at x
;to compute a derivative like x³ (3x²), we use Dg(x) = [g(x + dx) - g(x)] / dx

;like average-damp, deriv returns the derivative function of g
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

((deriv cube) 5);;75 

;g(x) = x - [g(x)/Dg(x)], alternative to the half-interval method using deriv to find a g(x) = 0
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

;iterates newtons-transform of a function with an initial guess until a fixed point is found 
(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt-newton x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))


;;Abstractions and first-class procedures

;up to now, sqrt functions were transformed to some other function to be computed
;we can, too, abstract this transformations
;it takes as arguments: a function, a transformation of the function and a guess
;it returns the fixed point of the transformation of the function
(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

;now we can call the general procedure with the function of transformation of our choosing
;sqrt-transform uses the average-damp
(define (sqrt-transform x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

;this one
(define (sqrt-newton-transform x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

























