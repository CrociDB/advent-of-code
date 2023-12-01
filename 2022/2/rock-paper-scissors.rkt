#lang racket

(define (rock-paper-scissors total)
 (define str (read-line (current-input-port) 'any))
 (cond
  [(string? str)
  (define-values (a b) (let ([l (map string (string->list str))]) (values (car l) (car (cdr (cdr l))))))
  (rock-paper-scissors (+ total (score a b)))]
  [else total]))


(define (score a b)
 (+
  (cond
   [(and (string=? a "A") (string=? b "Y")) 6]
   [(and (string=? a "A") (string=? b "Z")) 0]
   [(and (string=? a "B") (string=? b "X")) 0]
   [(and (string=? a "B") (string=? b "Z")) 6]
   [(and (string=? a "C") (string=? b "X")) 6]
   [(and (string=? a "C") (string=? b "Y")) 0]
   [else 3])
  (cond
   [(string=? b "X") 1]
   [(string=? b "Y") 2]
   [(string=? b "Z") 3])))

(rock-paper-scissors 0)

