(define (1+ a) (+ a 1))
(define (square a)(* a a))
(define (average a b)(/ (+ a b) 2))

; long way
(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (+ a 1) b))))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-sq (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; separating logic
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (iter-sum term a next)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a 0))

; new way

(define (sum-int a b)
  (define (identity x) x)
  (sum identity a 1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))

(define (pi-sum a b)
  (sum (lambda(i) (/ 1 (* i (+ i 2))))
       a
       (lambda(i) (+ i 4))
       b))

; alexander of huron square root

(define (_sqrt x)
  (fixed-point
   (lambda(y)(average(/ x y) y))
   1))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define average-damp
  (lambda(f)
    (lambda(x) (average(f x) x))))

; square root with average damping
(define (_sqrt x)
  (fixed-point
   (average-damp (lambda(y)(/ x y)))
   1))

; experiment with defining the function for divide
(define (_sqrt-2 X)
  (define (sq-calc z) (/ X z))
  (fixed-point
   (average-damp sq-calc)
   1))

; square root with newtons method

; dx
(define dx .000001)
; fixed-point
(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))
; newton
(define (newton f guess)
  (define df (deriv f)) ; just a simplification
  (fixed-point
   (lambda(x)(- x (/ (f x)(df x))))
   guess))
; question from class
(define (newton f guess)
  (fixed-point
   (lambda(x)(- x (/ (f x)((deriv f) x)))) ; the output of deriv of f is the procedure which will take x
   guess))
; deriv
(define deriv
  (lambda(f)
    (lambda(x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))
(define (_sqrt-3 x)
  (newton (lambda(y)(- x (square y)))
          1))
  
"square"
;(square 6)
"average"
;(average 1 3)
"sum-int"
;(sum-int 3 5)
"sum-sq"
;(sum-sq 3 4)
"pi-sum"
;(pi-sum 2 3)
"_sqrt"
;(_sqrt 4.0)
"_sqrt-2"
;(_sqrt-2 9.0)
"_sqrt-3"
(_sqrt-3 9.0)