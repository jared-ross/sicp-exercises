#lang sicp

(#%require racket/include)
(#%require (only racket/base print-as-expression print-mpair-curly-braces))
;; Show mcons as a normal list!
(print-as-expression #f)
(print-mpair-curly-braces #f)

(include "math.rkt")

; Ex-2.1

(define (make-rat n d)
  (let ((g (gcd n d))
        (n (* n (sign d)))
        (d (abs d)))
    (cons (/ n g)
          (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

; Ex-2.2

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start finish)
  (cons start finish))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment seg)
  (let ((x1 (x-point (start-segment seg)))
        (x2 (x-point (end-segment seg)))
        (y1 (y-point (start-segment seg)))
        (y2 (y-point (end-segment seg))))
    (make-point (average x1 x2) (average y1 y2))))

(define seg1
  (make-segment
   (make-point 0 0)
   (make-point 1 1)))

; (print-point (midpoint-segment seg1))

; The two corners/vertices: points
(define (make-rectangle v1 v2)
  (make-segment v1 v2))

(define (rect-point-select rect x-axis y-axis)
  (make-point
   (x-axis
    (x-point (start-segment rect))
    (x-point (end-segment rect)))
   (y-axis
    (y-point (start-segment rect))
    (y-point (end-segment rect)))))

(define (rect-bl rect)
  (rect-point-select rect min min))

(define (rect-br rect)
  (rect-point-select rect max min))

(define (rect-tl rect)
  (rect-point-select rect min max))

(define (rect-tr rect)
  (rect-point-select rect max max))

(define (rect-b rect)
  (make-segment (rect-bl rect) (rect-br rect)))

(define (rect-l rect)
  (make-segment (rect-bl rect) (rect-tl rect)))

(define (rect-t rect)
  (make-segment (rect-tl rect) (rect-tr rect)))

(define (rect-r rect)
  (make-segment (rect-tr rect) (rect-br rect)))

(define (length-segment seg)
  (let ((a (-
            (x-point (start-segment seg))
            (x-point (end-segment seg))))
        (b (-
            (y-point (start-segment seg))
            (y-point (end-segment seg)))))
    (sqrt (+ (square a) (square b)))))

(define (perim-rect rect)
  (+
   (length-segment (rect-t rect))
   (length-segment (rect-b rect))
   (length-segment (rect-l rect))
   (length-segment (rect-r rect))))

(define (area-rect rect)
  (* (length-segment (rect-t rect))
     (length-segment (rect-l rect))))

; Ex-2.4

(define (l-cons x y)
  (lambda (m) (m x y)))

(define (l-car z)
  (z (lambda (p q) p)))

(define (l-cdr z)
  (z (lambda (p q) q)))

; Ex-2.5

(define (23-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (23-car n)
  (define (iter n count)
    (if (divides? 2 n)
        (iter (/ n 2) (inc count))
        count))
  (iter n 0))

(define (23-cdr n)
  (define (iter n count)
    (if (divides? 3 n)
        (iter (/ n 3) (inc count))
        count))
  (iter n 0))


; Ex 2.6

(define church-zero (lambda (f) (lambda (x) x)))

(define (church-add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define church-one
  (lambda (f) (lambda (x) (f x))))

(define church-two
  (lambda (f) (lambda (x) (f (f x)))))

(define (church-add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))


(define (church-get n)
  ((n inc) 0))

; Ex 2.7 TODO Ex 2.11

(define (make-interval lower upper)
  (if (<= lower upper)
      (cons lower upper)
      (begin
        (display "Illegal Operation: Constructing an interval the \
                  wrong way round. ")
        (display (format-interval (cons lower upper)))
        (newline)
        false)))

(define (lower-bound-interval iv) (car iv))

(define (upper-bound-interval iv) (cdr iv))

(define (add-interval x y)
  (make-interval (+ (lower-bound-interval x) 
                    (lower-bound-interval y))
                 (+ (upper-bound-interval x) 
                    (upper-bound-interval y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound-interval x) 
                    (upper-bound-interval y))
                 (- (upper-bound-interval x) 
                    (lower-bound-interval y))))

; Signs for Intervals:
;
; +1: Sits only in the positives
;  0: Contains 0, or includes 0 in it's bounds
; -1: Sits only in the negatives
;
; To see how this works draw a chart of the operator, along side a
; chart of s.
(define (sign-interval iv)
  (let* ((l (lower-bound-interval iv))
        (u (upper-bound-interval iv))
        (s (sign (* u l))))
    (if (= s -1)
        0
        (* (sign l) s))))

(define (zero-bounds?-interval iv)
  (= 0
     (* (upper-bound-interval iv)
        (lower-bound-interval iv))))

(define (old-mul-interval x y)
    (let ((p1 (* (lower-bound-interval x) 
                 (lower-bound-interval y)))
          (p2 (* (lower-bound-interval x) 
                 (upper-bound-interval y)))
          (p3 (* (upper-bound-interval x) 
                 (lower-bound-interval y)))
          (p4 (* (upper-bound-interval x) 
                 (upper-bound-interval y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if
    (= 0 (sign-interval y))
      (display "Illegal Operation: Dividing by an interval containing zero")
      (mul-interval x 
                    (make-interval 
                     (/ 1.0 (upper-bound-interval y)) 
                     (/ 1.0 (lower-bound-interval y))))))

(define (width-interval iv)
  (/ (- (upper-bound-interval iv) 
        (lower-bound-interval iv))
     2))

(define (random-interval)
  (define width 5)
  (define granularity 0.25)
  (define (random-point)
    (let* ((num-granules (inexact->exact (/ width granularity)))
           (a-granule (random (+ num-granules 1)))
           (scaled-granule (* a-granule granularity))
           (scaled-and-shifted-granule (- scaled-granule (/ width 2))))
      scaled-and-shifted-granule))
  (let ((p1 (random-point))
        (p2 (random-point)))
         (make-interval (min p1 p2) (max p1 p2))))

(define (format-interval iv)
  (define $ number->string)
  (string-append
   "[" ($ (lower-bound-interval iv)) "," ($ (upper-bound-interval iv)) "]"))

(define (test-sign-mul-interval i1 i2)
  (define (print-is)
    (display (list 'mul (format-interval i1) (format-interval i2)))
    (display " => ")
    (display (format-interval (mul-interval i1 i2)))
    (newline)
    )
  (define (print-signs)
    (display (list 'mul (sign-interval i1) (sign-interval i2)))
    (display " => ")
    (display (sign-interval (mul-interval i1 i2)))
    (newline)
    )
  (define (error)
    (display "FAIL")
    (newline)
    (print-is)
    (print-signs)
    )
  (if (not (=
        (* (sign-interval i1) (sign-interval i2))
        (sign-interval (mul-interval i1 i2))))
      (error)
      (display "."))
  )
; (repeat-call (lambda () (test-sign-mul-interval (random-interval) (random-interval))) 10000)

(define (sign->symbol s)
  (cond
    ((= s  0)  0)
    ((= s  1) '+)
    ((= s -1) '-)))

(define (symbol->sign s)
  (cond
    ((eq? s  0)  0)
    ((eq? s '+)  1)
    ((eq? s '-) -1)))

; This took me a long time to do.
; I hope you appreciate this imagined reader of my code.
(define (mul-interval a b)
  ; (m)atch
  (let ((sa (sign-interval a))
        (sb (sign-interval b)))
    (define (signs a b)
        (and (= sa (symbol->sign a)) (= sb (symbol->sign b))))
    (define (pos i p)
      (if (eq? p 'u) (upper-bound-interval i) (lower-bound-interval i)))
    (define (mult pa pb) ; (p)osition of a/b, (u)pper or (l)ower
      (* (pos a pa) (pos b pb)))
    (cond
      ((signs '+ '+) (make-interval (mult 'l 'l) (mult 'u 'u)))
      ((signs '- '+) (make-interval (mult 'l 'u) (mult 'u 'l)))
      ((signs  0 '+) (make-interval (mult 'l 'u) (mult 'u 'u)))
      ((signs '+ '-) (mul-interval b a))
      ((signs '- '-) (make-interval (mult 'u 'u) (mult 'l 'l)))
      ((signs  0 '-) (make-interval (mult 'u 'l) (mult 'l 'l)))
      ((signs '+  0) (mul-interval b a))
      ((signs '-  0) (mul-interval b a))
      ((signs  0  0) (make-interval
                      (min (mult 'l 'u) (mult 'u 'l))
                      (max (mult 'l 'l) (mult 'u 'u))))
      )
    )
  )

(define (repeat-call f n)
  (f)
  (if (= n 1)
      nil
      (repeat-call f (- n 1)))
  )

(define (equal?-interval a b)
  (and
   (=
    (upper-bound-interval a)
    (upper-bound-interval b))
   (=
    (lower-bound-interval a)
    (lower-bound-interval b))))

(define (test-new-mul-interval)
  (define number-of-tests 1000)
  (define (error i1 i2)
    (newline)
    (display "Failed Match: ")
    (display (format-interval i1))
    (display " , ")
    (display (format-interval i2))
    (display " => ")
    (display (format-interval (old-mul-interval i1 i2)))
    (display " != ")
    (display (format-interval (mul-interval i1 i2)))
    (newline))
  (define (test)
    (let ((i1 (random-interval))
          (i2 (random-interval)))
      (if 
       (equal?-interval (old-mul-interval i1 i2) (mul-interval i1 i2))
       (display ".")
       (error i1 i2))
      ))
  (repeat-call test number-of-tests)
  )

;; (define (test-mul-interval)
;;   (define (should= a b m)
;;     (if (eq? a b)
;;         #t
;;         (begin 
;;           (display "FAIL: ")
;;           (display m)
;;           (newline)
;;           (display "Expected (= ")
;;           (display a)
;;           (display " ")
;;           (display b)
;;           (display ")")
;;           (newline))))
;;   (define (sign-rule a b ab)
;;     (let ((sa (sign-interval a))
;;           (sb (sign-interval b))
;;           (sab (sign-interval ab)))
;;       (cond
;;         ((= 1 (* sa sb))
;;          1)


;; Ex 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center-interval i)
  (/ (+ (lower-bound-interval i) 
        (upper-bound-interval i)) 
     2))

;; (define (width-interval i)
;;   (/ (- (upper-bound-interval i) 
;;         (lower-bound-interval i)) 
;;      2))

(define (make-center-percent c p)
  (make-center-width c (abs (* c p))))

(define (percent-interval i)
  (/ (width-interval i) (center-interval i)))

;; Ex 2.14
(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

(define (format-percent-interval iv)
  (define $ number->string)
  (string-append
   "Center: " ($ (center-interval iv)) ", Percent: " ($ (percent-interval iv))))

(define i-0-0.1 (make-center-percent 0 0.1))
(define i-0.5-0.1 (make-center-percent 0.5 0.1))
(define i-0.5-0.01 (make-center-percent 0.5 0.01))
(define i-1-0.5 (make-center-percent 1 0.5))
(define i-1-0.1 (make-center-percent 1 0.1))
(define i-1-0.01 (make-center-percent 1 0.01))
(define i-neg-1-0.1 (make-center-percent -1 0.1))


(define A (make-center-percent 50 0.1))
(define B (make-center-percent 20 0.1))

(define (test-pars)
  (display (format-percent-interval (par1 A B)))
  (newline)
  (display (format-percent-interval (par2 A B)))
  (newline))

(define (A/A)
  (let ((one (make-interval 1 1)))
    (display (format-percent-interval (div-interval A A)))
    (newline)
    (display (format-percent-interval (mul-interval A (div-interval one A))))))

(define (A/B) 
  (let ((one (make-interval 1 1)))
    (display (format-percent-interval (div-interval A B)))
    (newline)
    (display (format-percent-interval (mul-interval A (div-interval one B))))))

;; Ex 2.17
(define (last-pair l)
  (if (eq? (cdr l) '())
      l
      (last-pair (cdr l))))

(define (reverse l)
  (define (iter fw bk)
    (if (null? fw)
        bk
        (iter (cdr fw) (cons (car fw) bk))))
  (iter l '()))
  
