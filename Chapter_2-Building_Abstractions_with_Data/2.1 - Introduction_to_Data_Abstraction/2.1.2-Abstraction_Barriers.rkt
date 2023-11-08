#lang simply-scheme
;;helpers
(define (average x y) (/ (+ x y) 2))


;;2.1.2 - Abstraction Barriers

;;reducing rational to lowest terms upon access, rather than upon instantiation.
;;advantages: if we don't access the same rational number various times, we may be better off
;;by waiting until access time.
(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))


;;EXERCISES
;;2.2 - segments in a plane.
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y)
  (cons x y))

(define (make-segment first-point second-point)
  (cons first-point second-point))

(start-segment (make-segment (make-point 0 1) (make-point 4 1)))
(end-segment (make-segment (make-point 0 1) (make-point 4 1)))

(define (midpoint-segment segment)
  (cons (average (x-point (start-segment segment)) (x-point (end-segment segment)))
        (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(midpoint-segment(make-segment (make-point 2 3) (make-point 6 9)))


;;EXERCISE 2.3 - creating and interacting with rectangles
;;defining a rectangle in terms of make-segment
(define (create-rectangle length height)
  (make-segment (make-point 0 0) (make-point length height)))

(define (rectangle-area rectangle)
  (* (x-point (end-segment rectangle)) (y-point (end-segment rectangle))))

(define (rectangle-perimeter rectangle)
  (* 2 (+ (x-point (end-segment rectangle)) (y-point (end-segment rectangle)))))

(newline)
(rectangle-perimeter (create-rectangle 5 10))

;;defining a rectangle in terms of length and height
(define (make-rectangle length height) (cons length height))
(define (select-length rectangle) (car rectangle))
(define (select-height rectangle) (cdr rectangle))

(define (create-rectangle-variation length height)
  (make-rectangle length height))

(define (rectangle-area-variation rectangle)
  (* (select-length rectangle) (select-height rectangle)))

(define (rectangle-perimeter-variation rectangle)
  (* 2 (+ (select-length rectangle) (select-height rectangle))))

(rectangle-area-variation (make-rectangle 5 10))












