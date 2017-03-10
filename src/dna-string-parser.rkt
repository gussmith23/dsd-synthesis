#lang rosette

(require "dna-syntax.rkt")

(provide
 ; Returns a dna struct that represents the input string.
 ; Will match the semantics of the string format as
 ;   described in "Abstractions for DNA circuit design"
 ;   with the exception that domain names can only be integers.
 ; The behavior is UNDEFINED on input that doesn't match the said string format.
 ; Mutates the given symbol-table (see parameters section for more info)
 ;
 ; -- Parameters --
 ; input - A string in the format mentioned above to turn into a dna struct.
 string->species
         
 ; Returns a string that represents the input dna struct.
 ; Behaves as an inverse to string->species so will the result will
 ;   be in the string format as described in "Abstractions for DNA circuit design"
 ;
 ; -- Parameters --
 ; input - A valid dna struct representing the species to turn into a string.
 ;         Errors if not input isn't a valid dna struct
 species->string
 system->dsd
 dsd->system
)

(define (species->string input)
  (define (lookup id) (~a id))
  (define (to-string input)
    (match input
      [(? union? input) "??"]
      [(? expression? input) "??"]
      [(toehold id) (string-append (to-string id) "^")]
      [(complement id-or-toehold) (string-append (to-string id-or-toehold) "*")]
      [(upper-strand domain-list) (string-append "<" (to-string domain-list) ">")]
      [(lower-strand domain-list) (string-append "{" (to-string domain-list) "}")]
      [(duplex-strand domain-list) (string-append "[" (to-string domain-list) "]")]
      [(gate lu ll d rl ru)
       (let ([filter (lambda (s) (if (= (string-length s) 2) "" s))]) ; filter out <>, {}, and []
         (string-append (filter (to-string lu))
                        (filter (to-string ll))
                        (filter (to-string d))
                        (filter (to-string rl))
                        (filter (to-string ru))))]
      [(gate: g1 g2) (string-append (to-string g1) ":" (to-string g2))]
      [(gate:: g1 g2) (string-append (to-string g1) "::" (to-string g2))]
      [(list domains ...) (string-join (map to-string domains) " ")]
      [_ (lookup input)]))
  
  (if (valid-dna-struct? input)
      (to-string input)
      (error "Given invalid dna-struct")))

(define (string->species input)
  (cond
    [(string-contains? input "::") ; upper strand gate concatenation
     (let ([split-result (string-split input "::")])
       (foldl (lambda (substring accumulator)
                (gate:: accumulator (string->species substring)))
              (string->species (car split-result)) (cdr split-result)))]
    [(string-contains? input ":") ; lower strand gate concatenation
     (let ([split-result (string-split input ":")])
       (foldl (lambda (substring accumulator)
                (gate: accumulator (string->species substring)))
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
        [name (string-trim input (regexp "\\*|\\^") #:repeat? #t)])
    (define symbol (string->number name))
    (define inner-result (if is-toehold
                             (toehold symbol)
                             symbol))
    (if is-complement
        (complement inner-result)
        inner-result)
    ))

(define (parse-domain-string input)
  (filter
   (lambda (e) (not (null? e)))
   (map
    (lambda (input)
      (if (regexp-match (regexp "^\\?\\?$") input)
          (domain-hole)
          (parse-sequence-string input)))
    (string-split input))))

(define (parse-upper-strand-string input)
  (upper-strand (parse-domain-string (string-trim input (regexp "<|>")))))

(define (parse-lower-strand-string input)
  (lower-strand (parse-domain-string (string-trim input (regexp "{|}")))))

(define (parse-gate-string input)
  (let* ([components (string-split input (regexp "\\[|\\]") #:trim? #f)]
         [left-side (list-ref components 0)]
         [complex (list-ref components 1)]
         [right-side (list-ref components 2)])
    (gate
          (parse-upper-strand-string (car (or (regexp-match (regexp "<.*>") left-side) `(""))))
          (parse-lower-strand-string (car (or (regexp-match (regexp "{.*}") left-side) `(""))))
          (duplex-strand (parse-domain-string complex))
          (parse-lower-strand-string (car (or (regexp-match (regexp "{.*}") right-side) `(""))))
          (parse-upper-strand-string (car (or (regexp-match (regexp "<.*>") right-side) `(""))))
          )
    )
  )

(define (system->dsd system)
  (string-append
   "("
   (foldr
    (lambda (s1 s2)
      (if (equal? s2 "")
          (string-append s1 "\n)" s2)
          (string-append s1 "\n|" s2)))
    ""
    (map species->string system))))

(define (dsd->system dsd)
  (map string->species
       (string-split (string-trim dsd (regexp "\\(|\n\\)")) "\n|")))

