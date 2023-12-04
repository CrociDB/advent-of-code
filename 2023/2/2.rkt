#lang racket

(define bag (list '(12 r) '(13 g) '(14 b)))

(define (color str)
  (match str
    ["red" 'r]
    ["green" 'g]
    ["blue" 'b]))    

(define (parse-set str)
  (define cubes (string-split str ", "))
  (map (λ (c) (let ([v (string-split c " ")]) (list (string->number (car v)) (color (cadr v))))) cubes))

(define (parse-game str)
  (set! str (substring str 5))
  (define game (string-split str ":"))
  (define id (string->number (car game)))
  (define sets (string-split (cadr game) ";"))
  (list id (map parse-set sets)))

(define (get-color-number bag color)
  (if (empty? bag) 0 (let ([c (car bag)])
                       (if (eq? (cadr c) color) (car c) (get-color-number (cdr bag) color)))))

(define (check-game-bag game bag)
  (foldl (λ (g v) (let ([c (cadr g)] [n (car g)]) (and v (<= n (get-color-number bag c))))) #t game))
                
(define (check-game game bag)
  (if (foldl (λ (g v) (and v (check-game-bag g bag))) #t (cadr game)) (car game) 0))

(define (day2)
  (define (f c)
    (define str (read-line (current-input-port) 'any))
    (if (non-empty-string? str)
        (let ([l (check-game (parse-game str) bag)]) (f (+ c l)))
        c))
  (f 0))

(day2)
