(define nil '())

;;; Section 3.3.3 -- Tables

;;; One-dimensional tables

(define (lookup key table)
  (let ((record (_assq key (cdr table))))
    (if (null? record)
        nil
        (cdr record))))

(define (_assq key records)
  (cond ((null? records) nil)
        ((eq? key (caar records)) (car records))
        (else (_assq key (cdr records)))))

(define (insert! key value table)
  (let ((record (_assq key (cdr table))))
    (if (null? record)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))
        (set-cdr! record value)))
  'ok)

(define (_make-table)
  (list '*table*))

;;; Two-dimensional tables

(define (lookup key-1 key-2 table)
  (let ((subtable (_assq key-1 (cdr table))))
    (if (null? subtable)
        nil
        (let ((record (_assq key-2 (cdr subtable))))
          (if (null? record)
              nil
              (cdr record))))))

(define (insert! key-1 key-2 value table)
  (let ((subtable (_assq key-1 (cdr table))))
    (if (null? subtable)
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))
        (let ((record (_assq key-2 (cdr subtable))))
          (if (null? record)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))
              (set-cdr! record value)))))
  'ok)

;;; Local tables

(define (_make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (_assq key-1 (cdr local-table))))
        (if (null? subtable)
            nil
            (let ((record (_assq key-2 (cdr subtable))))
              (if (null? record)
                  nil
                  (cdr record))))))

    (define (insert! key-1 key-2 value)
      (let ((subtable (_assq key-1 (cdr local-table))))
        (if (null? subtable)
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))
            (let ((record (_assq key-2 (cdr subtable))))
              (if (null? record)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))
                  (set-cdr! record value)))))
       `ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

;;; The PUT and GET operations used in chapter 2

(define operation-table (_make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;;; Section 2.3.1

;;; Complex-number arithmetic

(define (+c z1 z2)
  (_make-rectangular (+ (_real-part z1) (_real-part z2))
                    (+ (_imag-part z1) (_imag-part z2))))

(define (-c z1 z2)
  (_make-rectangular (- (_real-part z1) (_real-part z2))
                    (- (_imag-part z1) (_imag-part z2))))

(define (*c z1 z2)
  (_make-polar (* (_magnitude z1) (_magnitude z2))
              (+ (_angle z1) (_angle z2))))

(define (/c z1 z2)
  (_make-polar (/ (_magnitude z1) (_magnitude z2))
              (- (_angle z1) (_angle z2))))

;;; Rectangular representation of complex numbers

(define (_make-rectangular x y) (cons x y))

(define (_real-part z) (car z))

(define (_imag-part z) (cdr z))

(define (_make-polar r a) 
  (cons (* r (cos a)) (* r (sin a))))

(define (_magnitude z)
  (sqrt (+ (square (car z)) (square (cdr z)))))

(define (_angle z)
  (atan (cdr z) (car z)))

;;; Polar representation of complex numbers

(define (_make-rectangular x y) 
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (_real-part z)
  (* (car z) (cos (cdr z))))

(define (_imag-part z)
  (* (car z) (sin (cdr z))))

(define (_make-polar r a) (cons r a))

(define (_magnitude z) (car z))

(define (_angle z) (cdr z))

;;; Section 2.3.2

;;; Manifest types

(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (if (not (atom? datum))
      (car datum)
      (error "Bad typed datum -- TYPE" datum)))

(define (contents datum)
  (if (not (atom? datum)) 
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))

;;; Complex numbers represented with manifest type

(define (rectangular? z)
  (eq? (type z) 'rectangular))

(define (polar? z)
  (eq? (type z) 'polar))

(define (_make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define (_make-polar r a)
  (attach-type 'polar (cons r a)))

(define (_real-part z)
  (cond ((rectangular? z) 
         (_real-part-rectangular (contents z)))
        ((polar? z)
         (_real-part-polar (contents z)))))

(define (_imag-part z)
  (cond ((rectangular? z)
         (_imag-part-rectangular (contents z)))
        ((polar? z)
         (_imag-part-polar (contents z)))))

(define (_magnitude z)
  (cond ((rectangular? z)
         (_magnitude-rectangular (contents z)))
        ((polar? z)
         (_magnitude-polar (contents z)))))

(define (_angle z)
  (cond ((rectangular? z)
         (_angle-rectangular (contents z)))
        ((polar? z)
         (_angle-polar (contents z)))))

(define (_real-part-rectangular z) (car z))

(define (_imag-part-rectangular z) (cdr z))

(define (_magnitude-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (_angle-rectangular z)
  (atan (cdr z) (car z)))

(define (_real-part-polar z)
  (* (car z) (cos (cdr z))))

(define (_imag-part-polar z)
  (* (car z) (sin (cdr z))))

(define (_magnitude-polar z) (car z))

(define (_angle-polar z) (cdr z))


;;; Section 2.3.3

;;; Note that in order to run this you need definitions of put and get
;;; (see section 3.3.3)

(put 'rectangular '_real-part _real-part-rectangular)
(put 'rectangular '_imag-part _imag-part-rectangular)
(put 'rectangular '_magnitude _magnitude-rectangular)
(put 'rectangular '_angle _angle-rectangular)

(put 'polar '_real-part _real-part-polar)
(put 'polar '_imag-part _imag-part-polar)
(put 'polar '_magnitude _magnitude-polar)
(put 'polar '_angle _angle-polar)

(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))
        (proc (contents obj))
        (error "Operator undefined for this type -- OPERATE"
               (list op obj)))))

(define (_real-part obj) (operate '_real-part obj))
(define (_imag-part obj) (operate '_imag-part obj))
(define (_magnitude obj) (operate '_magnitude obj))
(define (_angle obj) (operate '_angle obj))


