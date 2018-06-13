;; #lang sicp

; Util
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (double f)
  (compose f f))

(define (repeated f n)
  (define (iter acc count)
    (if (= count 0)
        acc
        (iter (compose f acc) (dec count))))
  (iter identity n))

(define (accumulate combiner null-value term a next b)
  (define (iter acc a)
    ;; (display (list 'iter acc a))
    ;; (newline)
    (if (> a b)
        acc
        (iter (combiner (term a) acc) (next a))))
  (iter null-value a))

(define (identity x) x)

(define sum (lambda (term a next b) (accumulate + 0 term a next b)))

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess )
        guess
        (iter (improve-guess guess))))
  iter)
; Analysis
(define tolerance 0.01)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (improve guess)
    (f guess))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (phi-fixed-point)
  (fixed-point
   (lambda (x)
     (/
      (+ 1 (/ 1 x) x)
      2))
   1.3))

(define (cont-frac n d k)
  (define (cont-frac-iter acc count)
    (if (> count 0)
        (cont-frac-iter (/ (n count)
                           (+
                            (d count)
                            acc))
                        (dec count))
        acc))
  (cont-frac-iter 0 k))

(define (phi-cont-frac)
  (/ 1
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             12)))

(define phi (phi-cont-frac))

(define (e-cont-frac)
  (define (n i) 1)
  (define (d i)
    (if (= (remainder i 3) 2)
        (- i (floor (/ i 3)))
        1))
  (+ 2 (cont-frac n d 10)))

(define e (e-cont-frac))

(define (tan-cont-frac x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac n d k))

(define (tan x) (tan-cont-frac x 20))

(define dx 0.001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1))

; Number Theoretic
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides? test-divisor n)
           test-divisor)
          (else (find-divisor
                 n
                 (next test-divisor)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fermat-test n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Useful Functions
;; (define (max x y)
;;   (if (< x y)
;;       y
;;       x))

;; (define (min . xs)
;;   (define (iter acc xs)
;;     (let ((x (car xs)))
;;       (if (x
;;           (iter
;;            (if (< acc (car xs))
;;                acc
;;                (car xs)))

(define (sign x)
  (cond
    ((> x 0)  1)
    ((= x 0)  0)
    ((< x 0) -1)))

(define (abs x)
  (* x (sign x)))

; Powers and Roots
(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (fast-expt b n)
  (define (iter a b n)
    (cond
      ((= n 0) a)
      ((not (even? n))
       (iter (* a b) b (- n 1)))
      (else
       (iter a (square b) (/ n 2)))))
  (iter 1 b n))

(define expt fast-expt)

; Misc Combinations
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; "Applications"
(define (pascals-triangle r c)
  (define (pasc r c)
    (cond ((and (= r 0) (= c 0)) 1)
          ((= r 0) 0)
          (else
           (+
            (pasc (- r 1) (- c 1))
            (pasc (- r 1) (+ c 1))))))
  (pasc r c))

(define (general-fib-iter a b p q count)
    (cond ((= count 0)
           b)
          ((even? count)
           (general-fib-iter a
                     b
                     ; p'
                     (+
                      (square p)
                      (square q))
                     ; q'
                     (+
                      (* 2 p q)
                      (square q))
                     (/ count 2)))
          (else
           (general-fib-iter (+ (* b q)
                        (* a q)
                        (* a p))
                     (+ (* b p)
                        (* a q))
                     p
                     q
                     (- count 1)))))

(define (fib n)
  (general-fib-iter 1 0 0 1 n))

; Trigonometry
(define (sine-by-thirding angle)
  (define (sine-third-angle x)
    (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.01))
      angle
      (sine-third-angle (sine (/ angle 3.0)))))

(define sine sine-by-thirding)

(define (cos x)
  (sqrt (- 1 (square (sine x)))))

