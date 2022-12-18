#lang racket

(struct monkey-program ([items #:mutable] op test run-true run-false [inspections #:mutable]) #:transparent)

; eval requires a namespace to work out of the repl
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (operation str) (eval (read (open-input-string str)) ns))

(define (parse-monkey-programs)
  (define (parse-monkey monkeys)
    (define str (read-line (current-input-port) 'any))
    (cond [(string? str)
            ; no need to get monkey name, because they're in order

            ; items
            (define items (regexp-split #rx"(, )" (cadr (regexp-split #rx"(: )" (read-line (current-input-port) 'any)))))
            (set! items (map string->number items))

            ; operation
            (define op-string (cadr (regexp-split #rx"(: )" (read-line (current-input-port) 'any))))

            ; the format seems to be always "new = old [op] [value]", so we can drop thre three first tokens
            (define tokenized (drop (regexp-split #rx" " op-string) 3))
            (define op (operation (car tokenized)))
            (define v (cadr tokenized))
            (define op-func (if (string-contains? v "old") (lambda (x) (op x x)) (lambda (x) (op x (string->number v)))))
            
            ; test
            (define test (string->number (cadr (regexp-split #rx"(Test: divisible by )" (read-line (current-input-port) 'any)))))
            (define test-lambda (lambda (n) (= 0 (modulo n test))))

            ; ... if true
            (define ntrue (string->number (cadr (regexp-split #rx"(If true: throw to monkey )" (read-line (current-input-port) 'any)))))

            ; ... if false
            (define nfalse (string->number (cadr (regexp-split #rx"(If false: throw to monkey )" (read-line (current-input-port) 'any)))))

            ; new line
            (read-line (current-input-port) 'any)

            (define m (monkey-program items op-func test-lambda ntrue nfalse 0))
            (parse-monkey (cons m monkeys))]

          [else monkeys]))
  (list->vector (reverse (parse-monkey null))))

(define (run-monkey monkeys i)
  (define m (vector-ref monkeys i))
  (set-monkey-program-inspections! m (+ (length (monkey-program-items m)) (monkey-program-inspections m)))
  (define (iter x)
    (define v (quotient ((monkey-program-op m) x) 3))
    (define move-to (if ((monkey-program-test m) v) (monkey-program-run-true m) (monkey-program-run-false m)))
    (define mm (vector-ref monkeys move-to))
    (set-monkey-program-items! mm (append (monkey-program-items mm) (list v)))
    (vector-set! monkeys move-to mm)
    mm)
  (map iter (monkey-program-items m))
  (set-monkey-program-items! m null)
  (vector-copy monkeys))

(define (run-round monkeys)
  (define (f i m) (run-monkey m i))
  (foldl f monkeys (range (vector-length monkeys))))

(define (run-many-rounds monkeys x)
  (foldl (lambda (_ m) (run-round m)) monkeys (range 20)))

(define (multiply-two-highest-inspections monkeys)
  (apply * (take (sort (vector->list (vector-map monkey-program-inspections monkeys)) >) 2)))

; run it!
(multiply-two-highest-inspections (run-many-rounds (parse-monkey-programs) 20))
