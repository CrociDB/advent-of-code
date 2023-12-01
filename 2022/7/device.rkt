#lang racket

(struct tree ([nodes #:mutable] [value #:mutable]) #:transparent)

(define (device-tree)
  (define root (tree null 0))
  (define (loop-input)
    (define str (read-line (current-input-port) 'any))
    (cond
      [(string? str)
       (cond
         [(string-contains? str "$ cd ..") root]
         [(string-contains? str "$ cd")
           (set-tree-nodes! root (cons (device-tree) (tree-nodes root)))
           (loop-input)]
         [(string-contains? str "$ ls")
           (loop-input)]
         [(not (string-contains? str "dir"))
           (set-tree-value! root (+ (tree-value root) (string->number (car (regexp-split #rx"[ ]" str)))))
           (loop-input)]
         [else (loop-input)])]
      [else root]))
  (loop-input))

(define (sum-nodes root)
  (if (empty? (tree-nodes root)) 0 (map sum-nodes (tree-nodes root)))
  (set-tree-value! root (+ (tree-value root) (apply + (map tree-value (tree-nodes root)))))
  root)

(define (nodes-max-value root)
  (let ([rv (tree-value root)])
    (apply + (cons (if (<= rv 100000) rv 0) (map nodes-max-value (tree-nodes root))))))

(nodes-max-value (sum-nodes (device-tree)))

