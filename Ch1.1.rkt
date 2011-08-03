"Methods"
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))
(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (_abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (_abs2 x)
  (cond ((< x 0) (- x))
        (else x)))
(define (_abs3 x)
  (if (< x 0)
      (- x)
      x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (_sqrt x)
  (sqrt-iter 1.0 x))


"Usage"
(* pi (* radius radius))
circumference
(square 4)
(square (square 3))
(f 5)
(_abs 10)
(_abs2 10)
(_abs3 10)
(_sqrt 4)

"Exercises"
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 1 5)))))
   (* 3 (- 6 2) (- 2 7)))

(define (add-square a b)
  (+ (* a a)(* b b)))
(define (add-three a b c)
  (cond ((and (< a b) (< a c)) add-square b c)
        ((and (< b a) (< b c)) add-square a c)
        ((and (< c a) (< c b)) add-square a b)))

(add-three 1 2 3)
(add-square 1 2)
"1.6"
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (_sqrt x)
  (sqrt-iter 1.0 x))
"This will never end << END 1.6"
"Start 1.7"
(define (sqrt-2 x)
  (define (try guess-new guess-old)
    (if (good-enough? guess-new guess-old)
        guess-new
        (try (improve guess-new) guess-new)))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (good-enough? a b)
    (< (abs (/ (- a b) b)) 0.001))
  (try 1.0 x))

(sqrt-2 1.0e-4)
; Value: 1.0000000025490743e-2

(sqrt-2 1.0e50)
; Value: 1.0000003807575104e25

"Start 1.8"
(define (cubed x)(* x x x))
(define (improve guess x)
   (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (cubed guess) x)) 0.001))
(define (curt-iter guess x)
  (if (good-enough? guess x)
      guess
      (curt-iter (improve guess x)
                 x)))
(define (curt x)
  (curt-iter 1.0 x))

(curt 8)