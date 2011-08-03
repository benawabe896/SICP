(define (+rat x y)
  (make-rat
   (+ (* (numer x)(denom y))
      (* (numer y)(denom x)))
   (* (denom x)(denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x)(numer y))
   (* (denom x)(denom y))))

(define (make-rat n d)
  (cons n d))

(define (numer x)(car x))
(define (denom x)(cdr x))
; let's try it out now
; 1/2 + 1/4

(define a (make-rat 1 2))
(define b (make-rat 1 4))

(define answer (+rat a b))

(numer answer)
(denom answer)

; part 3 in video

(define (average a b)(/ (+ a b) 2))
(define (square a)(* a a))

; test out manual implementation of cons, car and cdr
(define (_cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))
(define (_car x)(x 1))
(define (_cdr x)(x 2))

; represeting vectors in the plane
(define (_make-vector x y)(_cons x y))
(define (xcor p)(_car p))
(define (ycor p)(_cdr p))

; representing line segments
(define (make-seg p q)(_cons p q))
(define (seg-start s)(_car s))
(define (seg-end s)(_cdr s))

(define (midpoint s)
  (let ((a (seg-start s))
        (b (seg-end s)))
    (_make-vector
     (average (xcor a)(xcor b))
     (average (ycor a)(ycor b)))))

(define (segment_length s)
  (let
      ((dx (- (xcor (seg-end s))
              (xcor (seg-start s))))
       (dy (- (ycor (seg-end s))
              (ycor (seg-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))

; let me try some implementations
(define vectorx (_make-vector 1 2))
(define vectory (_make-vector 2 3))
(define segment (make-seg vectorx vectory))
(segment_length segment)
