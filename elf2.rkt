#lang racket

(define (count-elf total maximum)
    (define str (read-line (current-input-port) 'any))
    (cond
        [(string? str)
            (define b (string->number str))
            (cond
            [b
                (count-elf (+ b total) maximum)]
            [else
                (count-elf 0 (make-maximum total maximum))])]
        [else (apply + (make-maximum total maximum))]))

(define (make-maximum value maxlist)
    (take (sort (cons value maxlist) >) (min 3 (+ 1 (count (lambda (x) #t) maxlist)))))

(count-elf 0 null)
