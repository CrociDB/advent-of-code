#lang racket

(require racket/set)

(struct vec (x y) #:transparent)
(struct rope (head tail) #:transparent)

; main functions to get results

(define (bridge rp pos)
  (define str (read-line (current-input-port) 'any))
  (cond [(string? str)
          (define run-input (let ([x (regexp-split #rx"[ ]" str)])
            (build-list (string->number (cadr x))
                              (lambda (_) (car (string->list (car x)))))))
          (define res (foldl (lambda (m r)
                   (define rope-newhead (rope (move-end (rope-head (car r)) (list m 1)) (rope-tail (car r))))
                   (define n (rope (rope-head rope-newhead) (sum-vec (rope-tail (car r)) (find-tail-movement rope-newhead))))
                   (list n (cons (rope-tail n) (cadr r)))) (list rp null) run-input))
          
          (bridge (car res) (append (cadr res) pos))]
        [else (values rp pos)]))

(define (rope-bridge) 
  (define-values (r lst) (bridge (rope (vec 0 0) (vec 0 0)) null))
  (set-count (list->set lst)))
  
; util funcs

(define (sign x)
  (if (< x 0) - +))

(define (clamp x)
  (min (max x -1) 1))

(define (sum-vec vec1 vec2)
  (vec (+ (vec-x vec1) (vec-x vec2)) (+ (vec-y vec1) (vec-y vec2))))

; exercise funcs

(define (move-end e i)
  (let ([ins (car i)] [val (cadr i)])
    (sum-vec e
             (cond
               [(char=? ins #\R) (vec val 0)]
               [(char=? ins #\U) (vec 0 (- val))]
               [(char=? ins #\L) (vec (- val) 0)]
               [(char=? ins #\D) (vec 0 val)]))))

(define (dist rope p)
  (- (p (rope-head rope)) (p (rope-tail rope))))

(define (find-tail-movement rope)
  (let ([dx (dist rope vec-x)] [dy (dist rope vec-y)])
    (cond [(and (< (+ (abs dx) (abs dy)) 2) (not (= (abs dx) (abs dy)))) (vec 0 0)]
          [(and (= (abs dx) (abs dy)) (<= (+ (abs dx) (abs dy)) 2) (vec 0 0))]
          [(and (>= (abs dx) 2) (= dy 0)) (vec ((sign dx) 1) 0)]
          [(and (>= (abs dy) 2) (= dx 0)) (vec 0 ((sign dy) 1))]
          [else (vec (clamp dx) (clamp dy))])))
      
; run it!
(rope-bridge)

