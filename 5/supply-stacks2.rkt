#lang racket

(define (sort-stacks vec)
  (define vec (get-stacks (make-vec)))
  (read-line (current-input-port) 'any)
  (sort-stack-line vec)
  (get-final-answer vec))

(define (get-final-answer vec)
  (define top (map
    (lambda (l) (if (empty? l) #\ (string-ref (first (reverse l)) 1)))
    (vector->list vec)))
  (string-normalize-spaces (list->string top)))
  

(define (sort-stack-line vec)
  (define str (read-line (current-input-port) 'any))
  (cond
    [(string? str)
      (define new-vec (sort-line vec str))
      (sort-stack-line new-vec)]
    [else vec]))

(define (sort-line vec str)
  (define ins (drop (regexp-split #rx"(move )|( from )|( to )" str) 1))
  (define move-amount (string->number (car ins)))
  (define move-from (- (string->number (cadr ins)) 1))
  (define move-to (- (string->number (caddr ins)) 1))

  (define remove-list (vector-ref vec move-from))
  (define append-list (vector-ref vec move-to))

  (define new-append-list (append append-list (reverse (take (reverse remove-list) move-amount))))
  (define new-remove-list (reverse (drop (reverse remove-list) move-amount)))

  (vector-set! vec move-from new-remove-list)
  (vector-set! vec move-to new-append-list)
  vec)

(define (make-vec)
  (define vec (make-vector 10))
  (vector-fill! vec '())
  vec)

(define (get-stacks vec)
  (define str (read-line (current-input-port) 'any))
  (cond
    [(string-contains? str "[")
      (define new-vec (get-stack-line vec (string->list str) 0))
      (get-stacks new-vec)]
    [else vec]))

(define (get-stack-line vec line count)
  (cond
    [(empty? line) vec]
    [(char=? (car line) #\space) (get-stack-line vec (cdr line) (+ 1 count))]
    [else
      (define str (list->string (take line 3)))
      (define pos (quotient count 4))
      (vector-set! vec pos (cons str (vector-ref vec pos)))
      (get-stack-line vec (drop line 3) (+ 3 count))]))
  
  
(sort-stacks (make-vec))

