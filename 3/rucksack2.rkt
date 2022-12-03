#lang racket

(define (rucksacks total)
  (let ([g (get-group)])
    (cond
      [(member 'eof g) total]
      [else
        (define-values (v1 v2 v3) (values (car g) (cadr g) (caddr g))) 
        (rucksacks (+ total (char-code (car (set-intersect v1 v2 v3)))))])))

(define (get-group)
  (map
    (lambda (x)
      (let ([str (read-line (current-input-port) 'any)])
        (if (string? str) (string->list str) 'eof))) '(0 1 2)))

(define (char-code chr)
  (let* ([i (char->integer #\a)] [c (char->integer chr)])
    (- (if (< c i) (+ c 58) c) (- i 1))))

(rucksacks 0)
