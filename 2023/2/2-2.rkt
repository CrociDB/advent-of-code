#lang racket

(define bag (list '(r) '(g) '(b)))

(define (color str)
  (match str
    ["red" 'r]
    ["green" 'g]
    ["blue" 'b]))    

(define (parse-set str)
  (define cubes (string-split str ", "))
  (map (λ (c) (let ([v (string-split c " ")]) (list (string->number (car v)) (color (cadr v))))) cubes))

(define (parse-game str)
  (define game (string-split str ":"))
  (define sets (string-split (cadr game) ";"))
  (map parse-set sets))

(define (get-color-number bag color)
  (if (empty? bag) 0 (let ([c (car bag)])
                       (if (eq? (cadr c) color) (car c) (get-color-number (cdr bag) color)))))

(define (make-bag-match gm min)
  (for ([g gm])
    (let ([bag-value (hash-ref min (cadr g))])
      (when (or (null? bag-value) (> (car g) bag-value))
          (hash-set! min (cadr g) (car g)))))
  min)
  
(define (make-bag game)
  (foldl make-bag-match (make-hash bag) game))

(define (mul-bag bag)
  (* (hash-ref bag 'r) (hash-ref bag 'g) (hash-ref bag 'b)))

(define (day2)
  (define (f c)
    (define str (read-line (current-input-port) 'any))
    (if (non-empty-string? str)
        (let ([l (mul-bag (make-bag (parse-game str)))]) (f (+ c l)))
        c))
  (f 0))

(day2)

