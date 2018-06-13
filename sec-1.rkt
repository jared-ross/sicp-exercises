#lang racket

; Ex 1.3

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (max x y)
  (if (< x y)
      y
      x))

(define (min x y)
  (if (> x y)
      y
      x))

(define (wo-min x y z)
  (let ([m (min (min x y) z)])
    (cond ((= m x) (list y z))
          ((= m y) (list x z))
          (else (list x y)))))

(define (ex-1.3 x y z)
  (let* ([l (wo-min x y z)]
        [x (car l)]
        [y (cadr l)])
    (sum-of-squares x y)))

;; (ex-1.3 1 2 3)
;; (ex-1.3 3 1 2)
;; (ex-1.3 1 3 1)
;; (ex-1.3 1 1 1)
;; (ex-1.3 1 3 2)
;; (ex-1.3 2 3 1)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (ex-1.11-recur n)
  (define (f n)
    (cond ((< n 3) n)
          (else (+ (f (- n 1))
                   (* 2 (f (- n 2)))
                   (* 3 (f (- n 3)))))))
  (f n))

(define (range a b step)
  (if (< a b)
      (cons a (range (+ a step) b step))
      '()))

;; (map ex-1.11-recur (range 0 10 1))

(define (ex-1.11-iter n)
  (define (f fn-1 fn-2 fn-3 count)
    (let ([fn (+ fn-1 (* 2 fn-2) (* 3 fn-3))])
      (if (= count 0)
          fn
          (f fn fn-1 fn-2 (- count 1)))))
  (if (< n 3)
      n
      (f 2 1 0 (- n 3))))

;; (map ex-1.11-iter (range 0 10 1))

(define (ex-1.12 r c)
  (define (pasc r c)
    (cond ((and (= r 0) (= c 0)) 1)
          ((= r 0) 0)
          (else
           (+
            (pasc (- r 1) (- c 1))
            (pasc (- r 1) (+ c 1))))))
  (pasc r c))


;; (require racket/pretty)

;; (pretty-print
;;  (map
;;   (lambda (r)
;;     (map (lambda (c) (ex-1.12 r c)) (range -9 10 1)))
;;   (range 0 10 1)))

(define (!= a b)
  (not (= a b)))

(define (print-tabs count)
  (and (> count 0)
       (display "  ")
       (print-tabs (- count 1))))

(define (depth-print depth el)
  (print-tabs depth)
  (display el)
  (display "\n"))

(define cc-count 0)
(define (count-change amount)
  (set! cc-count 0)
  (cc amount 5 0)
  cc-count)

(define (inc x)
  (+ x 1))

(define (cc amount kinds-of-coins depth)
  (set! cc-count (inc cc-count))
  (depth-print depth (list 'cc amount kinds-of-coins))
  (cond ((= amount 0)
         (and
          ;(depth-print depth (list '=> 1))
          1)
         )
        ((or (< amount 0)
             (= kinds-of-coins 0))
         (and
          ;(depth-print depth (list '=> 0))
          0)
         )
        (else
         (+ (cc amount (- kinds-of-coins 1) (+ depth 1))
            (cc (- amount (first-denomination
                           kinds-of-coins))
                kinds-of-coins
                (+ depth 1))))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (map count-change (range 0 50 1))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.01))
       angle
       (p (sine (/ angle 3.0)))))

(define (ex-1.16 b n)
  (define (exp a b n)
    (println (list 'exp a b n))
    (cond
      ((= n 0) a)
      ((not (even? n))
       (exp (* a b) b (- n 1)))
      (else
       (exp a (square b) (/ n 2)))))
  (exp 1 b n))

(define (ex-1.17 a b)
  (define (double x)
    (+ x x))

  (define (halve x)
    (/ x 2))

  (define (* a b r)
    (println (list '* a b r))
    (cond
      ((= b 0) 0)
      ((= b 1)
       (+ a r))
      ((not (even? b))
       (* a (- b 1) (+ r a)))
      (else
       (* (double a) (halve b) r))))

  (let
      ([a (max a b)]
       [b (min a b)])
    (* a b 0)))


(define (ex-1.19 n)
  (define (square x)
    (* x x))
  (define (fib-iter a b p q count)
    (cond ((= count 0)
           b)
          ((even? count)
           (fib-iter a
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
           (fib-iter (+ (* b q)
                        (* a q)
                        (* a p))
                     (+ (* b p)
                        (* a q))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))


;; '(b * p + a * q)

;; (define (nth n l)
;;   (if (or (> n (length l)) (< n 0))
;;     (error "Index out of bounds.")
;;     (if (eq? n 0)
;;       (car l)
;;       (nth (- n 1) (cdr l)))))

;; (define (without x l)
;;   (define (without-iter acc x l)
;;     (println (list 'without-iter acc x l))
;;     (cond
;;         ((eq? l '()) acc)
;;         ((eq? x (car l)) (without-iter acc x (cdr l)))
;;         (else (without-iter (cons (car l) acc) x (cdr l)))))
;;   (without-iter '() x l))

;; (define (split x sexpe
;;   (define (split-iter acc l r x)
;;     (cond
;;       ((eq? r '()) (cons l acc))
;;       ((eq? (car r) '())
;;        (split-iter acc l (cdr r) x))
;;       ((eq? x (car r))
;;        (split-iter (cons l acc) '() (cdr r) x))
;;       (else
;;        (split-iter acc (cons (car r) l) (cdr r) x))))
;;   (reverse
;;    (split-iter '() '() sexp x)))

;; (define (alg-parse expr)
;;   (define stages '(+ * into-brackets =))
;;   (define (parser tree stage)
;;     (cond
;;       ((= stage 0)
;;        (list '+ (split '+ tree)))))

;;   (alg-parse expr 0))

;; (alg-parse '( a * b + c * d + e * f )



(define (average l)
  (define (sum l)
    (foldl (lambda (acc x) (+ acc x)) 0 l))
  (/ (sum l) (length l)))

(define (smallest-divisor n)
  (find-divisor n 2))

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

(define (divides? a b)
  (= (remainder b a) 0))

(define (runtime) (current-inexact-milliseconds))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test prime? n)
  ;(newline)
  ;(display n)
  (start-prime-test prime? n (runtime)))

(define (start-prime-test prime? n start-time)
  (if (prime? n)
      (report-prime n (- (runtime)
                       start-time))
      (void)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes prime? a b)
  (cond ((>= a b) (void))
        ((even? a)
         (search-for-primes prime? (add1 a) b))
        (else
         (timed-prime-test prime? a)
         (search-for-primes prime? (+ a 2) b))))

(define (ex-1.23)
  (begin
    (search-for-primes prime? 1000 1020)
    (search-for-primes prime? 10000 10038)
    (search-for-primes prime? 100000 100050)
    (search-for-primes prime? 1000000 1000050)
    (search-for-primes prime? 10000000 10000110)))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (ex-1.24)
  (let ([prime? (lambda (n) (fast-prime? n 10))])
    (search-for-primes prime? 1000 1020)
    (search-for-primes prime? 10000 10038)
    (search-for-primes prime? 100000 100050)
    (search-for-primes prime? 1000000 1000050)
    (search-for-primes prime? 10000000 10000110)))


(define tolerance 0.01)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (println (list 'trying guess 'next next))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (ex-1.35)
  (fixed-point (lambda (x) (/ (+ 1 (/ 1 x) x) 2)) 1.3))

(define (ex-1.36)
  (fixed-point
   (lambda (x)
     (/
      (+
       (/ (log 1000) (log x))
       x)
      2)) 1.5))

(define (cont-frac n d k)
  (define (cont-frac-iter acc count)
    (println (list 'cont-frac-iter acc count))
    (if (> count 0)
        (cont-frac-iter (/ (n count)
                           (+
                            (d count)
                            acc))
                        (sub1 count))
        acc))
  (cont-frac-iter 0 k))

(define (ex-1.37)
  (/ 1
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             12)))

(define (cont-frac-recur n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (add1 i))))))
  (recur 1))

(define (ex-1.38-e)
  (define (n i) 1)
  (define (d i)
    (if (= (remainder i 3) 2)
        (- i (floor (/ i 3)))
        1))
  (+ 2 (cont-frac n d 10)))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac n d k))

(define ($ x)
  (exact->inexact x))

;($ (tan-cf (/ 3.141592 4) 10))

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

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) a)))

; (newtons-method (cubic 2 4 2) 1)

;; (define (double f)
;;   (lambda (x)
;;     (f (f x))))

;; (define (inc x)
;;   (add1 x))

; (((double (double double)) inc) 0)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (double f)
  (compose f f))

;; (define (repeated f n)
;;   (define (repeated-iter f r count)
;;     (cond
;;       ((= 0 count) (compose r f))
;;       ((even? count)
;;         (repeated-iter (double f) r (/ count 2)))
;;       (else (repeated-iter f (compose f r) (sub1 count)))))
;;   (repeated-iter f identity n))

(define (doubling-method double combine base-val)
  (define (iter f r count)
    (println (list 'iter f r))
    (cond
      ((= 0 count) r)
      ((even? count)
        (iter (double f) r (/ count 2)))
      (else (iter f (combine f r) (sub1 count)))))
  (lambda (f n) (iter f base-val n)))

(define (repeated f n)
  (define (iter acc count)
    (if (= count 0)
        acc
        (iter (compose f acc) (sub1 count))))
  (iter identity n))

;(define (repeated f n))

;(define repeated (doubling-method double compose identity))

(define fast-power (doubling-method square * 1))

; ex-1.44
(require math/statistics)


(define (smooth f)
  (lambda (x)
    (average (list (f (- x dx)) (f x) (f (+ x dx))))))

(define (nth-fold-smooth f n)
  ((repeated smooth n) f))

(define (average-damp f)
  (lambda (x)
    (average (list x (f x)))))

; ex-1.45
(define (nth-root x n)
  (define fixed-point-start 1)
  (fixed-point
   ((repeated average-damp n)
    (lambda (y) (/ x (fast-power y n))))
   fixed-point-start))

(define (root-function x n)
  (lambda (y) (/ x (fast-power y (- n 1)))))


(define (test-root x n m t)
  (fixed-point
   ((repeated average-damp m)
    (root-function x n))
   t))

(define (accumulate combiner null-value term a next b)
  (define (iter acc a)
    (if (> a b)
        null-value
        (iter (combiner (term a) acc) (next a))))
  (iter null-value a))

(define (identity x) x)

(define sum (lambda (term a next b) (accumulate + 0 term a next b)))

; These don't always converge for an initial guess of 2

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess )
        guess
        (iter (improve-guess guess))))
  iter)



(define (sqrt x)
  (define tolerance 0.001)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 5)
