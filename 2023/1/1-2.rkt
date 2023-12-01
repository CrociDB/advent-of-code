#lang racket
(require srfi/13)

(define (drop-str str n) (list->string (drop (string->list str) n)))

(define (spelled-number str)
  (define numbers (list "zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
  (let ([n (filter-map (λ (n)
                (let ([c (string-contains str n)])
                  (if (and c (= (string-contains str n) 0))
                      (index-of numbers n)
                      #f)))
              numbers)])
    (if (not (null? n)) (car n) #f)))

(define (digit c)
  (and (>= (char->integer c) (char->integer #\0)) (<= (char->integer c) (char->integer #\9))))

(define (digit-first str)
  (if (non-empty-string? str)
      (let ([c (car (string->list str))])
        (if (digit c) (list->string (list c)) #f))
      #f))

(define (digits-first-last str)
  (string->number (list->string (list (car (string->list str)) (car (reverse (string->list str)))))))

(define (numbers str)
  (define (f str nstr)
    (cond
      [(non-empty-string? str)
        (define df (digit-first str))
        (if df
           (f (drop-str str 1) (string-append nstr df))
           (let ([sn (spelled-number str)])
             (if sn
                 (f (drop-str str 1) (string-append nstr (number->string sn)))
                 (f (drop-str str 1) nstr))))]
      [else nstr]))
  (digits-first-last (f str "")))
      

(define (day1)
  (define (f c)
    (define str (read-line (current-input-port) 'any))
    (if (non-empty-string? str)
        (let ([l (numbers str)]) (f (+ c l)))
        c))
  (f 0))

(day1)
