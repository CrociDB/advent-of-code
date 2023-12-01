#lang racket

(struct tcpu (cycle x strength-values) #:transparent)

(define (new-cpu) (tcpu 0 1 null))

(define (run-instruction cpu instruction)
  (case (car instruction)
    [("noop") (cycle cpu 1)]
    [("addx") (cpu-addx (cycle cpu 2) (string->number (cadr instruction)))]))

(define (cycle cpu cycles)
  (foldl (lambda (_ ncpu)
           (define c (+ 1 (tcpu-cycle ncpu)))
           (define cycle-cicled (modulo (- c 20) 40))
           (define sv-list (tcpu-strength-values ncpu))
           (define sv (if (= cycle-cicled 0) (cons (* (tcpu-x ncpu) c) sv-list) sv-list))
           (tcpu c (tcpu-x ncpu) sv)) cpu (range cycles)))

(define (cpu-addx cpu x)
  (tcpu (tcpu-cycle cpu) (+ x (tcpu-x cpu)) (tcpu-strength-values cpu)))

(define (sum-strength-values cpu)
  (apply + (tcpu-strength-values cpu)))


; deal with input
(define (crt cpu)
  (define str (read-line (current-input-port) 'any))
  (cond [(string? str)
          (define ins (regexp-split #rx"[ ]" str))
          (crt (run-instruction cpu ins))]
        [else cpu]))


; run it!
(sum-strength-values (crt (new-cpu)))
