#lang racket

(define (parse-winning line)
  (define hands (map (λ (h) (filter-map string->number (string-split h " "))) (string-split (cadr (string-split line ":")) "|")))
  (define common (filter-map (λ (v) (if (index-of (cadr hands) v) v #f)) (car hands)))
  (floor (expt 2 (- (length common) 1))))

(define (day4)
  (define (f c)
    (define str (read-line (current-input-port) 'any))
    (if (non-empty-string? str)
        (let ([l (parse-winning str)]) (f (+ c l)))
        c))
  (f 0))

(day4)

