#lang racket

(define (rock-paper-scissors total)
 (define str (read-line (current-input-port) 'any))
 (cond
  [(string? str)
  (define-values (a b) (let ([l (map string (string->list str))]) (values (car l) (car (cdr (cdr l))))))
  (rock-paper-scissors (+ total (score2 a b)))]
  [else total]))
  ; first exercise
(define (score1 a b)
 (+ (list-ref (rotate '(3 0 6) (letter-index b)) (letter-index a)) (list-ref '(1 2 3) (letter-index b))))

  ; seconds exercise
  (define (score2 a b)
   (let ([c (pick-hand a b)])
    (+ (list-ref (rotate '(3 0 6) (letter-index c)) (letter-index a)) (list-ref '(1 2 3) (letter-index c)))))

(define (pick-hand a b)
 (list-ref (rotate '("C" "A" "B") (- 3 (letter-index b))) (letter-index a)))

  (define (rotate l n) (let ([nn (- 3 n)]) (append (drop l nn) (take l nn))))
(define (letter-index x)
 (cond
  [(or (string=? x "A") (string=? x "X")) 0]
  [(or (string=? x "B") (string=? x "Y")) 1]
  [else 2]))

(rock-paper-scissors 0)

