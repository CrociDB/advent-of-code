#lang racket

(define (create-matrix matrix)
  (define str (read-line (current-input-port) 'any))
  (cond [(string? str)
          (create-matrix (cons (list->vector (map (compose string->number string) (string->list str))) matrix))]
        [else matrix]))

(define (max l) (foldl (lambda (x a) (if (> x a) x a)) 0 l))

(define (split-line m c l)
  (list
    (take (vector->list (vector-ref m c)) l)
    (drop (vector->list (vector-ref m c)) (+ l 1))))

(define (split-column m c l)
  (let ([ls (vector->list (vector-map (lambda (mx) (vector-ref mx l)) m))])
    (list
      (take ls c)
      (drop ls (+ c 1)))))

(define (check-visibility m x y)
  (define w (- (vector-length m) 1))
  (define h (- (vector-length (vector-ref m 0)) 1))
  (cond [(or (= x 0) (= y 0) (= x w) (= y h)) 'edge]
        [else
          (define value (vector-ref (vector-ref m x) y))
          (> (apply + (map (lambda (z) (if (> value z) 1 0)) (map max (append (split-line m x y) (split-column m x y))))) 0)]))

(define (check-visibility-all m)
  (define w (vector-length m))
  (define h (vector-length (vector-ref m 0)))
  (length (filter identity (map (lambda (i) (check-visibility m (car i) (cadr i))) (cartesian-product (range w) (range h))))))

; run it!
(check-visibility-all (list->vector (reverse (create-matrix null))))