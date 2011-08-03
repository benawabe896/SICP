(define (1+ a) (+ a 1))
(define (square a)(* a a))
(define (average a b)(/ (+ a b) 2)) 
(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (_sqrt x)
  (newton (lambda(y)(- x (square y)))
          1))

(define (newton f guess)
  (define df (deriv f))
  (fixed-point
   (lambda(x)(- x (/ (f x)(df x))))
   guess))

(define deriv
  (lambda(f)
    (lambda(x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define dx .00001)

(_sqrt 4)