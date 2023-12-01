#lang racket

(define (get-marker sl count)
  (define l (take sl 4))
  (define sum (apply + (map (lambda (x) (length (filter (lambda (z) (char=? x z)) l))) l)))
  (if (= sum 4) (+ count 4) (get-marker (cdr sl) (+ count 1))))

(get-marker (string->list (read-line (current-input-port) 'any)) 0)

