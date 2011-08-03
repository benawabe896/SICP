(define (1+ a) (+ a 1))
(define (square a)(* a a))
(define (average a b)(/ (+ a b) 2))

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

(define (_sqrt x)
  (fixed-point
   (average-damp (lambda(y)(/ x y)))
   1))

"square"
(square 6)
"average"
(average 1 3)
"sum-int"
(sum-int 3 5)
"sum-sq"
(sum-sq 3 4)
"pi-sum"
(pi-sum 2 3)
"_sqrt"
(_sqrt 2)