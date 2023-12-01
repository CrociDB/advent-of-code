#lang racket

(define (camp-cleanup total)
  (define str (read-line (current-input-port) 'any))
  (cond
    [(string? str)
      (define-values (s1 s2) (get-range-sets str))
      (define-values (ns1 ns2) (set-excluding-intersect s1 s2))
      (camp-cleanup (+ total (if (or (empty? ns1) (empty? ns2)) 1 0)))]
    [else total]))

(define (get-range-sets line)
  (let ([v (regexp-split #rx"[-,]" line)])
    (values
      (inclusive-range (string->number (car v)) (string->number (cadr v)))
      (inclusive-range (string->number (caddr v)) (string->number (cadddr v))))))

(define (set-excluding-intersect s1 s2)
  (define intersect (set-intersect s1 s2))
  (values (set-subtract s1 intersect) (set-subtract s2 intersect)))

(camp-cleanup 0)
