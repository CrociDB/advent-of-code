#lang racket

(define (count-elf total maximum)
   (define str (read-line (current-input-port) 'any))
   (cond
      [(string? str)
         (define b (string->number str))
         (cond
            [b (count-elf (+ b total) maximum)]
            [else
               (if (= 0 total) maximum (count-elf 0 (if (> total maximum) total maximum)))])]
      [else maximum]))

(count-elf 0 0)
