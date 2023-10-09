#lang simply-scheme
;;helper procedures
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;;2.1.1 - Example: Arithmetic Operations for Rational Numbers
;;simple arithmetic abstractions with rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mult-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;;PAIRS
(define x1 (cons 1 2))

;(car x1); 1
;(cdr x1); 2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

x
y
z


;;Representing rational numbers
;;reducing numerator and numerator to lowest terms, 6/3 (gcd = 3) = 2/1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x));; car
  (display "/")
  (display (denom x)));; cdr

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-half one-third))

(print-rat (mult-rat one-half one-third))

(print-rat (add-rat one-third one-third))


;;EXERCISES
;;2.1
;;if numerator and denominator are positive they continue being so
;;else, numerator is negative and denominator is positive.
(define (make-rat-normalizer n d)
  (if (and (positive? n) (positive? d))
      (cons n d)
      (cons
       (if (< n 0)
           n
           (- n))
       (if (< d 0)
           (* -1 d)
           d))))
















