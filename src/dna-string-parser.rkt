#lang rosette

(provide string->species)

; Attempts to parse the given input into the representation defined
; by the grammar in dna-syntax.rkt
; Undefined on input that doesn't follow the syntax described in
; "Abstractions for DNA circuit design" paper (as in, it can do anything)
(define (string->species input)
  (cond
    [(string-contains? input "::") ; upper strand gate concatenation
     (let ([split-result (string-split input "::")])
       (foldl (lambda (substring accumulator)
                (list `:: accumulator (string->species substring)))
              (string->species (car split-result)) (cdr split-result)))]
    [(string-contains? input ":") ; lower strand gate concatenation
     (let ([split-result (string-split input ":")])
       (foldl (lambda (substring accumulator)
                (list `: accumulator (string->species substring)))
              (string->species (car split-result)) (cdr split-result)))]
    [(string-contains? input "[") ; indicates complex (gate)
     (parse-gate-string input)]
    [(string-contains? input "<") ; indicates upper strand
     (parse-upper-strand-string input)]
    [(string-contains? input "{") ; indicates lower strand
     (parse-lower-strand-string input)]
    ))

(define (parse-sequence-string input)
  (let ([is-complement (string-contains? input "*")]
        [is-toehold (string-contains? input "^")]
        [symbol (string->symbol (string-trim input (regexp "\\*|\\^") #:repeat? #t))])
    (define inner-result (if is-toehold
                             `(T ,symbol)
                             `(S ,symbol)))
    (if is-complement
        (cons `C inner-result)
        inner-result)
    ))

(define (parse-domain-string input)
  (filter (lambda (e) (not (null? e))) (map parse-sequence-string (string-split input))))

(define (parse-upper-strand-string input)
  (list `U (parse-domain-string (string-trim input (regexp "<|>")))))

(define (parse-lower-strand-string input)
  (list `L (parse-domain-string (string-trim input (regexp "{|}")))))

(define (parse-gate-string input)
  (let* ([components (string-split input (regexp "\\[|\\]") #:trim? #f)]
         [left-side (list-ref components 0)]
         [complex (list-ref components 1)]
         [right-side (list-ref components 2)])
    (list `gate
          (parse-upper-strand-string (car (or (regexp-match (regexp "<.*>") left-side) `(""))))
          (parse-lower-strand-string (car (or (regexp-match (regexp "{.*}") left-side) `(""))))
          (parse-domain-string complex)
          (parse-lower-strand-string (car (or (regexp-match (regexp "{.*}") right-side) `(""))))
          (parse-upper-strand-string (car (or (regexp-match (regexp "<.*>") right-side) `(""))))
          )
    )
  )