#lang racket

(struct tcpu (cycle x video-memory) #:transparent)

(define (new-cpu) (tcpu 0 1 null))

(define (run-instruction cpu instruction)
  (case (car instruction)
    [("noop") (cycle cpu 1)]
    [("addx") (cpu-addx (cycle cpu 2) (string->number (cadr instruction)))]))

(define (cycle cpu cycles)
  (foldl (lambda (_ ncpu)
           (define c (tcpu-cycle ncpu))
           (define pos (modulo c 40))
           (define pixel (if (and (>= (tcpu-x ncpu) (- pos 1)) (<= (tcpu-x ncpu) (+ pos 1))) #\# #\.))
           (define vm (cons pixel (tcpu-video-memory ncpu)))
           (tcpu (+ 1 c) (tcpu-x ncpu) vm)) cpu (range cycles)))

(define (cpu-addx cpu x)
  (tcpu (tcpu-cycle cpu) (+ x (tcpu-x cpu)) (tcpu-video-memory cpu)))

(define (print-video-memory cpu)
  (define memory (reverse (tcpu-video-memory cpu)))
  (define (print-line m)
    (cond [(empty? m) m]
          [else
            (define x (min 40 (length m)))
            (display (list->string (take m x)))
            (display "\n")
            (print-line (drop m x))]))
  (print-line memory))


; deal with input
(define (crt cpu)
  (define str (read-line (current-input-port) 'any))
  (cond [(string? str)
          (define ins (regexp-split #rx"[ ]" str))
          (crt (run-instruction cpu ins))]
        [else cpu]))


; run it!
(print-video-memory (crt (new-cpu)))
