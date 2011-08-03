(define (1+ a) (+ a 1))
(define (square a)(* a a))
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

(define (sum-int a b)
  (define (identity x) x)
  (sum identity a 1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))

(sum-int 3 5)
(sum-sq 3 4)