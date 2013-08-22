#lang scheme
 
(provide (all-defined-out))

;; string * string -> list
;; splits a string delimited by ch
;; eg (split-string "he:l:l" ":") returns '("he" "l" "l")
(define (split-string str ch)
  (define (do-split str str-acc lst-acc cur-index)
    (cond [(>= cur-index (string-length str)) 
           ;;non-empty list means we have encountered seperator so add left over str
           (if (empty? lst-acc) lst-acc (append lst-acc (list str)))]
          [(string=? ch (substring str cur-index (+ cur-index 1))) 
           (do-split (substring str (+ cur-index 1))  "" (append lst-acc (list str-acc)) 0)]
          [else (do-split str (string-append str-acc (substring str cur-index (+ cur-index 1))) lst-acc (+ cur-index 1))]))
  (do-split str "" '() 0))

  
;; builds a http query string
(define (http-build-query lst)
  (define (build-query lst acc)
    (if (null? lst) acc (build-query (cdr lst) (string-append acc (caar lst) "=" (car (cdr (car lst))) "&"))))
  (let ([query (build-query lst "")])
    (substring query 0 (- (string-length query) 1))))
    
  
(define (build-string lst sep)
  (define (build lst acc)
    (if (null? lst) acc (build (cdr lst) (string-append acc sep (car lst)))))
    (substring (build lst "") (string-length sep)))  
  
  
(define (flatten lst)
  (define (do-flatten lst acc)
    (cond [(null? lst) acc]
          [(list? (car lst)) (append acc (do-flatten (car lst) '()))]
          [else (do-flatten (cdr lst) (append acc (list (car lst))))]))
  (do-flatten lst '()))
  
  (define (my-substring sb str)
    (define (do-substring sb str acc)
      (cond [(empty? sb) #t]
            [(empty? str) #f]
            [(char=? (car sb) (car str)) 
             (do-substring (cdr sb) (cdr str) (append acc (list (car sb))))]
            [else (do-substring (append acc sb) (cdr str) '())])) ;start over
    (do-substring (string->list sb) (string->list str) '()))
  
  
(define (file->string-list filename)
  (let ((port (open-input-file filename)))
    (let lp ((all-lines '()))
      (let ((line-or-eof (read-line port)))
        (if (eof-object? line-or-eof)
            (reverse all-lines)
            (lp (cons line-or-eof all-lines)))))))
  
(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
           (let loop ()
             body ...
             (if condition (loop) #f)))))
  
(define-syntax for
  (syntax-rules (in as)
    ((for element in list body ...)
     (map (lambda (element)
            body ...)
          list))
    ((for list as element body ...)
     (map (lambda (element)
            body ...)
          list))))

(define (depth lst)
  (define (my-depth lst acc)
    (if (null? lst) acc (my-depth (cdr lst) (+ acc 1))))
  (my-depth lst 0))

(define (rec-depth lst)
  (define (my-rec-depth lst acc)
    (cond [(null? lst) acc]
          [(list? (car lst)) (my-rec-depth (cdr lst) (+ acc (my-rec-depth (car lst) 0)))]
          [else (my-rec-depth (cdr lst) (+ acc 1))]))
  (my-rec-depth lst 0))
  
;(call-with-output-file "db" (lambda (out) (write '(1 2 3 4) out)) 'replace)
  
