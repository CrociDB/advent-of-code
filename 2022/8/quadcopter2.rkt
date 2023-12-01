#lang racket

(define (create-matrix matrix)
  (define str (read-line (current-input-port) 'any))
  (cond [(string? str)
          (create-matrix (cons (list->vector (map (compose string->number string) (string->list str))) matrix))]
        [else matrix]))

(define (max l) (foldl (lambda (x a) (if (> x a) x a)) 0 l))

(define (take-until-first p l)
  (define met #f)
  (define (predicate x)
    (define oldmet met)
    (set! met (if met #t (p x)))
    (if oldmet #f x))
  (filter-map predicate l)) 
  
(define (split-line m c l)
  (define value (vector-ref (vector-ref m c) l))
  (define (predicate x) (>= x value))
  (list
    (reverse (take-until-first predicate (reverse (take (vector->list (vector-ref m c)) l))))
    (take-until-first predicate (drop (vector->list (vector-ref m c)) (+ l 1)))))

(define (split-column m c l)
  (define value (vector-ref (vector-ref m c) l))
  (define (predicate x) (>= x value))
  (let ([ls (vector->list (vector-map (lambda (mx) (vector-ref mx l)) m))])
    (list
      (reverse (take-until-first predicate (reverse (take ls c))))
      (take-until-first predicate (drop ls (+ c 1))))))

(define (check-visibility m x y)
  (define w (- (vector-length m) 1))
  (define h (- (vector-length (vector-ref m 0)) 1))
  (cond [(or (= x 0) (= y 0) (= x w) (= y h)) #f]
        [else
          (define value (vector-ref (vector-ref m x) y))
          (foldl * 1 (map length (append (split-line m x y) (split-column m x y))))]))

(define (check-visibility-all m)
  (define w (vector-length m))
  (define h (vector-length (vector-ref m 0)))
  (max (filter-map (lambda (i) (check-visibility m (car i) (cadr i))) (cartesian-product (range w) (range h)))))

; run it!
(check-visibility-all (list->vector (reverse (create-matrix null))))
