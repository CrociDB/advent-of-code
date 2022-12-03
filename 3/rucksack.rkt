#lang racket

(define (rucksacks total)
  (define str (read-line (current-input-port) 'any))
  (cond
    [(string? str)
      (define-values (p1 p2) (divide-string str))
      (rucksacks (+ total (char-code (car (set-intersect p1 p2)))))]
    [else total]))

(define (divide-string str)
  (let* ([len (string-length str)] [mid (/ len 2)])
    (values (string->list (substring str 0 mid)) (string->list (substring str mid len)))))

(define (char-code chr)
  (let* ([i (char->integer #\a)] [c (char->integer chr)])
    (- (if (< c i) (+ c 58) c) (- i 1))))

(rucksacks 0)
