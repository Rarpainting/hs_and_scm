(define add1
  (lambda (x)
    (+ x 1)))
(define sub1
  (lambda (x)
    (- x 1)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? a (car lat)) #t)
     (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons
            (car lat)
            (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (firsts (cdr l)))
     (else (cons
            (car (car l))
            (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons
                           (old)
                           (cons
                            (new)
                            (insertR (cdr lat)))))
     (else (insertR new old (cdr lat))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons
                           (new)
                           (cons
                            (old)
                            (insertL (cdr lat)))))
     (else (insertL new old (cdr lat))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons
                           (new)
                           (subst (cdr lat))))
     (else (subst new old (cdr lat))))))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

(define *
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons
            (o+ (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (a b)
    (cond
     ((zero? a) #f)
     ((zero? b) #t)
     (else (> (sub1 a) (sub1 b))))))

(define <
  (lambda (a b)
    (cond
     ((zero? b) #f)
     ((zero? a) #t)
     (else (< (sub1 a) (sub1 b))))))

(define =
  (lambda (a b)
    (cond
     ((> a b) #f)
     ((< a b) #f)
     (else #t))))

(define Expt
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (* (Expt n (sub1 m)) n)))))

(define quot
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (div (o- n m) m))))))

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 length (cdr l))))))

(define no-nums
  (lambda (l)
    (cond
     ((null? l) '())
     ((number? (car l)) (no-nums (cdr l)))
     (else (cons
            (car l)
            (no-nums (cdr l)))))))

(define all-nums
  (lambda (l)
    (cond
     ((null? l) '())
     ((number? (car l)) (cons
                         (car l)
                         (all-nums (cdr l))))
     (else (all-nums (cdr l))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define pick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((one? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((one? n) (cdr lat))
     (else (cons
            (car lat)
            (rempick (sub1 n) (cdr lat)))))))

;;;;;; Chapter 5 ;;;;;;;;

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? a (car l)) (rember* (cdr l)))
                       (else (cons
                              (car l)
                              (rember* (cdr l))))))
     (cons
      (rember* (car l))
      (rember* (cdr l))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     (atom? (car l)) (cond
                      ((eq? old (car l)) (cons
                                          (old)
                                          (cons
                                           (new)
                                           (insertR* new old (cdr l)))))
                      (else (cons
                             (car l)
                             (insertR* new old (cdr l)))))
     (else (cons
            (insertR* new old (car l))
            (insertR* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     (atom? (car l)) (cond
                      ((eq? old (car l)) (cons
                                          (new)
                                          (cons
                                           (old)
                                           (insertL* new old (cdr l)))))
                      (else (cons
                             (car l)
                             (insertL* new old (cdr l)))))
     (else (cons
            (insertL* new old (car l))
            (insertL* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
                       ((eq? a (car l)) (add1
                                         (occur* a (cdr l))))
                       (else (occur* a (cdr l)))))
     (else (o+
            (occur* a (car l))
            (occur* a (cdr l)))))))
