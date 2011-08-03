(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
"Start 1.8"
(define (cubed x)(* x x x))
(define (improve guess x)
   (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (cubed guess) x)) 0.1))
(define (curt-iter guess x)
  (if (good-enough? guess x)
      guess
      (curt-iter (improve guess x)
                 x)))
(define (curt x)
  (curt-iter 1.0 x))


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

(define (cubicroot x)
  (define (try guess-new guess-old)
    (if (good-enough? guess-new guess-old)
        guess-new
        (try (improve guess-new) guess-new)))
  (define (good-enough? a b)
    (< (abs (/ (- a b) b)) 0.001))
  (define (improve y)
    (/ (+ (/ x (square y))
          (* 2 y))
       3))
  (try 1.0 x))


(define (calculate x improve)
  (define (try guess-new guess-old)
    (if (good-enough? guess-new guess-old)
        guess-new
        (try (improve x guess-new) guess-new)))
  (define (good-enough? a b)
    (< (abs (/ (- a b) b)) 0.001))
  (try 1.0 x))

(define (sqrt-3 x)
  (calculate x (lambda(x y)
    (/ (+ y (/ x y)) 2))))
  
(define (cubicroot-2 x)
  (calculate x (lambda(x y)
    (/ (+ (/ x (square y))
          (* 2 y))
       3))))
  
         
(sqrt-2 4)
(sqrt-3 9)
(cubicroot 8)
(cubicroot-2 64)