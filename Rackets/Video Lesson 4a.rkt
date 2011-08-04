(define (atom? x) ; atom? is not in a pair or null (empty)
(and (not (pair? x))
(not (null? x))))

;;; Scheme code from the Pattern Matcher lecture

;; Pattern Matching and Simplification

(define (match pattern expression dictionary)
  (cond ((eq? dictionary 'failed) 'failed)
        ((atom? pattern)
         (if (atom? expression)
             (if (eq? pattern expression)
                 dictionary
                 'failed)
             'failed))
        ((arbitrary-constant? pattern)
         (if (constant? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ((arbitrary-variable? pattern)
         (if (variable? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ((arbitrary-expression? pattern)
         (extend-dictionary pattern expression dictionary))
        ((atom? expression) 'failed)
        (else
         (match (cdr pattern)
                (cdr expression)
                (match (car pattern)
                       (car expression)
                       dictionary)))))

(define (instantiate skeleton dictionary)
  (cond ((atom? skeleton) skeleton)
        ((skeleton-evaluation? skeleton)
         (evaluate (evaluation-expression skeleton)
                   dictionary))
        (else (cons (instantiate (car skeleton) dictionary)
                    (instantiate (cdr skeleton) dictionary)))))

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp   (car exp))
              (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dictionary (match (pattern (car rules))
                                   exp
                                   (make-empty-dictionary))))
            (if (eq? dictionary 'failed)
                (scan (cdr rules))
                (simplify-exp (instantiate (skeleton (car rules))
                                           dictionary))))))
    (scan the-rules))
  simplify-exp)

;; Dictionaries

(define (make-empty-dictionary) '())

(define (extend-dictionary pat dat dictionary)
  (let ((vname (variable-name pat)))
    (let ((v (assq vname dictionary)))
      (cond ((null? v)
             (cons (list vname dat) dictionary))
            ((eq? (cadr v) dat) dictionary)
            (else 'failed)))))

(define (lookup var dictionary)
  (let ((v (assq var dictionary)))
    (if (null? v)
        var
        (cadr v))))

;; Expressions

(define (compound? exp) (pair?   exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom?   exp))

;; Rules

(define (pattern  rule) (car  rule))
(define (skeleton rule) (cadr rule))

;; Patterns

(define (arbitrary-constant?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?c) false))

(define (arbitrary-expression?  pattern)
  (if (pair? pattern) (eq? (car pattern) '? ) false))

(define (arbitrary-variable?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?v) false))

(define (variable-name pattern) (cadr pattern))

;; Skeletons & Evaluations

(define (skeleton-evaluation?    skeleton)
  (if (pair? skeleton) (eq? (car skeleton) ':) false))

(define (evaluation-expression evaluation) (cadr evaluation))


;; Evaluate (dangerous magic)

(define (evaluate form dictionary)
  (if (atom? form)
      (lookup form dictionary)
      (apply (EVAL (lookup (car form) dictionary)
                   user-initial-environment)
             (mapcar (lambda (v) (lookup v dictionary))
                     (cdr form)))))

;;
;; A couple sample rule databases...
;;

;; Algebraic simplification

(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                (: (op c1 c2))                )
    ( ((? op) (?  e ) (?c c ))                ((: op) (: c) (: e))          )
    ( (+ 0 (? e))                             (: e)                         )
    ( (* 1 (? e))                             (: e)                         )
    ( (* 0 (? e))                             0                             )
    ( (* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2))
                                                 (* (: e1) (: e3)))         )
    ))

(define algsimp (simplifier algebra-rules))

;; Symbolic Differentiation

(define deriv-rules
  '(
    ( (dd (?c c) (? v))              0                                 )
    ( (dd (?v v) (? v))              1                                 )
    ( (dd (?v u) (? v))              0                                 )
    ( (dd (+ (? x1) (? x2)) (? v))   (+ (dd (: x1) (: v))
                                        (dd (: x2) (: v)))             )
    ( (dd (* (? x1) (? x2)) (? v))   (+ (* (: x1) (dd (: x2) (: v)))
                                        (* (dd (: x1) (: v)) (: x2)))  )
    ( (dd (** (? x) (?c n)) (? v))   (* (* (: n) (+ (: x) (: (- n 1))))
                                        (dd (: x) (: v)))              )
    ))

(define dsimp (simplifier deriv-rules))

(define scheme-rules
  '(( (square (?c n)) (: (* n n)) )
    ( (fact 0) 1 )
    ( (fact (?c n)) (* (: n) (fact (: (- n 1)))) )
    ( (fib 0) 0 )
    ( (fib 1) 1 )
    ( (fib (?c n)) (+ (fib (: (- n 1)))
                      (fib (: (- n 2)))) )
    ( ((? op) (?c e1) (?c e2)) (: (op e1 e2)) ) ))

(define scheme-evaluator (simplifier scheme-rules))

(dsimp (* 1 (+ 1 2) 2 3))