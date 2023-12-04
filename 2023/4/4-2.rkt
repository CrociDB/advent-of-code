#lang racket

(define (parse-winning line)
  (define hands (map (λ (h) (filter-map string->number (string-split h " "))) (string-split (cadr (string-split line ":")) "|")))
  (define common (filter-map (λ (v) (if (index-of (cadr hands) v) v #f)) (car hands)))
  (length common))

(define (fake-hash-ref! hash key ret)
  (if (hash-has-key? hash key) (hash-ref hash key) ret))

(define (day4)
  (define multiplier (make-hash))
  (define i 0)
  (define (f c)
    (define str (read-line (current-input-port) 'any))
    (if (non-empty-string? str)
        (let ([winning (parse-winning str)] [currmul (fake-hash-ref! multiplier i 1)])
          (define sum 0)
          (when (> winning 0)
            (for ([v (range (+ i 1) (+ i winning 1))])
              (hash-set! multiplier v (+ (fake-hash-ref! multiplier v 1) currmul))))
            (set! sum (+ sum currmul))
          (set! i (+ i 1))
          (f (+ c sum)))
        c))
  (f 0))

(day4)

