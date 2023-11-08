#lang simply-scheme
;;helper functions
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;;2.1.3 - What is Meant by Data?
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))


;;EXERCISES
;;2.4 - define cdr following the cons implementation
(define (cons-exercise x y)
  (lambda (m) (m x y)))

(define (car-exercise z)
  (z (lambda (p q) p)))

;;using substitution model cons returns a procedure that applies an argument m to 5 10
;;we call cdr that returns (lambda (m) (m x y)) to (lambda (p q) q))
;;which substitutes the argument m, whence applying the procedure ((lambda (p q) q) 5 10) = 10
(define (cdr-exercise z)
  (z (lambda (p q) q)))


;;2.5 - representing integers as pairs
;;constructor
(define (make-pair a b)
  (cons (expt 2 a) (expt 3 b)))

;;getters
(define (first-integer pair)
  (define (iter current result)
    (if (not (= (remainder current 2) 0))
        result
        (iter (/ current 2) (+ result 1))))
  (iter (car pair) 0))

(define (second-integer pair)
  (define (iter current result)
    (if (not (= (remainder current 3) 0))
        result
        (iter (/ current 3) (+ result 1))))
  (iter (cdr pair) 0))


;;2.6 -
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))










