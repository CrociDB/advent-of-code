#lang racket

(define file-system-size 70000000)
(define file-system-necessary 30000000)

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

(define (delete-smallest-directory root)
  (define free-space (- file-system-size (tree-value root)))
  (define space-to-free (- file-system-necessary free-space))
  ((free space-to-free) root))

(define ((free space) root)
  (car (sort (filter (lambda (x) (> x space)) (flatten (flatten-values root))) <)))

(define (flatten-values root)
  (flatten (cons (tree-value root) (map flatten-values (tree-nodes root)))))
 
(define (flatten l)
  (cond
    [(empty? l) null]
    [(not (list? l)) (list l)]
    [else (append (flatten (first l)) (flatten (rest l)))]))


; Finally run!
(delete-smallest-directory (sum-nodes (device-tree)))

