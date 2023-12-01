#lang racket

(define (digits-first-last str)
  (let ([d (filter-map (λ (x)
         (if (and
              (>= (char->integer x) (char->integer #\0))
              (<= (char->integer x) (char->integer #\9))) x #f))
       (string->list str))])
    (string->number (list->string (list (car d) (car (reverse d)))))))

(define (day1)
  (define (f c)
    (define str (read-line (current-input-port) 'any))
    (if (non-empty-string? str)
        (let ([l (digits-first-last str)]) (f (+ c l)))
        c))
  (f 0))

(day1)
