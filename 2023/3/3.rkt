#lang racket

(define (expand-range r)
  (list (max (- (car r) 1) 0) (cdr r)))

(define (num-ranges line)
  (set! line (string-append line "."))
  (define numbers (remove-duplicates (filter non-empty-string? (regexp-split #rx"[^0-9]" line))))
  (define full-ranges (map (λ (n) (list (string->number n) (regexp-match-positions* (regexp (string-append "[^0-9]?(" n ")[^0-9\n]")) line #:match-select cadr))) numbers))
  ;(displayln full-ranges)
  (define ranges '())
  (for* ([r full-ranges] [nr (cadr r)])
    (set! ranges (append ranges (list (list (car r) (expand-range nr))))))
  ranges)

(define (range-in-line range line)
  (if (regexp-match #rx"[^0-9.\r\n]" (substring line (car range) (min (string-length line) (+ 1 (cadr range))))) #t #f))

(define (day3)
  (define ranges '())
  (define all-lines '())
  (define (f)
    (define str (read-line (current-input-port) 'any))
    (if (non-empty-string? str)
        (let ([l (num-ranges str)])
          (set! ranges (append ranges (list l)))
          (set! all-lines (append all-lines (list str)))
          (f))
        0))
  (f)
  (define sum 0)
  (for ([r ranges] [c (length all-lines)])
    (let* ([l (drop all-lines (- c (if (> c 0) 1 0)))] [lines (take l (min (if (= c 0) 2 3) (length l)))])
      
      (define a (map (λ (i) (if (foldl (λ (j acc) (or acc (range-in-line (cadr i) j))) #f lines) (car i) 0)) r))
      (set! sum (+ sum (apply + a)))
      a))
  sum)

(day3)

