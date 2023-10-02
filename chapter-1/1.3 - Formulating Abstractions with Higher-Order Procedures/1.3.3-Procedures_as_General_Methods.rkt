#lang simply-scheme
;;1.3.3 - Procedures as General Methods
;;helper procedures
(define (cube n) (* n n n))
(define (square n) (* n n))
(define (average a b) (/ (+ a b) 2))

;;1.3.3 Procedures as General Methods
;;Finding roots of equations by the half-interval method

; f(a) < 0 < f(b)
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a)) (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (positive? a-value) (negative? b-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

;;half interval of x3 - 2x - 3 = 0, between 1 and 2
(half-interval-method
 (lambda (x) (- (* x x x) (* 2 x) 3))
 1.0 2.0)


;;Finding fixed points of functions
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;fixed point of the function y = sin y + cos y
(fixed-point
 (lambda (y) (+ (sin y) (cos y)))
 1.0)

;;square root y2 = x, y = x/y
(define (sqrt x)
  (fixed-point 
   (lambda (y) (average y (/ x y)))
   1.0))

(provide fixed-point)


;;EXERCISES
; Exercise 1.35: Show that the golden ratio φ
; (1.2.2) is a fixed point of the transformation x↦1+1/x , and use this fact to compute φ
; by means of the fixed-point procedure.
(fixed-point
 (lambda (x) (+ 1 (/ 1 x)))
 1.6180)