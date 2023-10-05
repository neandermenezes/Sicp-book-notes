#lang simply-scheme
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define a 3) ;; a = 3
(define b (+ a 1)) ;; b = 4
(+ a b (* a b))

(/
 (+ 5 4
    (- 2
       (- 3
          (+ 6
             (/ 4 5)))))
 (* 3
    (- 6 2)
    (- 2 7)))

(define (sum_square_two_greatest x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y z) (< y x)) (sum-of-squares z x))
        ((and (< z y) (< z x)) (sum-of-squares y x))))

(define (factorial n)
  (cond ((= n 1) 1)
        (else (* n (factorial (- n 1))))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (p) (p))

;;(define (test x y) 
;;  (if (= x 0) 
;;      0 
;;     y))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (is-bigger-than-zero x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0 (- x)))))
  

