#lang racket

(define message-size 14)

(define (get-marker sl count)
  (define l (take sl message-size))
  (define sum (apply + (map (lambda (x) (length (filter (lambda (z) (char=? x z)) l))) l)))
  (if (= sum message-size) (+ count message-size) (get-marker (cdr sl) (+ count 1))))

(get-marker (string->list (read-line (current-input-port) 'any)) 0)

