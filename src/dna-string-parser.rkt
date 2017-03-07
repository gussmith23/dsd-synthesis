#lang rosette

(require "dna-syntax.rkt")

(provide
 ; Returns a dna struct that represents the input string.
 ; Will match the semantics of the string format as
 ;   described in "Abstractions for DNA circuit design".
 ; The behavior is UNDEFINED on input that doesn't match the said string format.
 ; Mutates the given symbol-table (see parameters section for more info)
 ;
 ; -- Parameters --
 ; input - A string in the format mentioned above to turn into a dna struct.
 ; symbol-table - A mutable mapping from names that appear in the input to integers
 ;                  representing them.
 ;                If there isn't a mapping for a name, a mapping will be added
 ;                  such that the name will map to an integer that is unique from
 ;                  all the other integers mapped in the symbol-table.
 ;                By default uses a default global symbol table which is the same one
 ;                  that species->string uses
 string->species
         
 ; Returns a string that represents the input dna struct.
 ; Behaves as an inverse to string->species so will the result will
 ;   be in the string format as described in "Abstractions for DNA circuit design"
 ;
 ; -- Parameters --
 ; input - A valid dna struct representing the species to turn into a string.
 ;         Errors if not input isn't a valid dna struct
 ; symbol-table - A 1-1 mapping between friendly name strings to integers.
 ;                Every integer in input will be represented in the result
 ;                  as the friendly name string that maps to that integer.
 ;                Errors if an integer in input has no corresponding name.
 ;                By default uses the same default global symbol table as string->species.
 species->string
 system->dsd
 dsd->system
)

; The default symbol table used by string->species and species->string
; These symbol tables map strings to integers
; The strings are the friendly names and the integers are 
(define default-symbol-table (make-hash))

(define (species->string input [symbol-table default-symbol-table])
  (define inverse-symbol-table (make-hasheq)) ; inverse hash table will let us lookup id to get pretty string
  (define (lookup id) (hash-ref inverse-symbol-table id
                                    (thunk (if (integer? id) (~a id) (error "Id not found:" id)))))
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
  (begin
    (hash-for-each symbol-table
                   (lambda (str id)
                     (hash-set! inverse-symbol-table id str)))
    (if (valid-dna-struct? input)
        (to-string input)
        (error "Given invalid dna-struct"))))

(define (string->species input [symbol-table default-symbol-table])
  (cond
    [(string-contains? input "::") ; upper strand gate concatenation
     (let ([split-result (string-split input "::")])
       (foldl (lambda (substring accumulator)
                (gate:: accumulator (string->species substring symbol-table)))
              (string->species (car split-result) symbol-table) (cdr split-result)))]
    [(string-contains? input ":") ; lower strand gate concatenation
     (let ([split-result (string-split input ":")])
       (foldl (lambda (substring accumulator)
                (gate: accumulator (string->species substring symbol-table)))
              (string->species (car split-result) symbol-table) (cdr split-result)))]
    [(string-contains? input "[") ; indicates complex (gate)
     (parse-gate-string input symbol-table)]
    [(string-contains? input "<") ; indicates upper strand
     (parse-upper-strand-string input symbol-table)]
    [(string-contains? input "{") ; indicates lower strand
     (parse-lower-strand-string input symbol-table)]
    ))

(define (parse-sequence-string input symbol-table)
  (let ([is-complement (string-contains? input "*")]
        [is-toehold (string-contains? input "^")]
        [name (string-trim input (regexp "\\*|\\^") #:repeat? #t)])
    (define symbol
      (if (hash-has-key? symbol-table name)
          (hash-ref symbol-table name)
          ; creates a new-id to be one higher than the max integer in the symbol-list
          (let ([new-id (+ 1
                           (argmax identity (cons -1 (hash-values symbol-table)))
                           )])
            (hash-set! symbol-table name new-id)
            new-id)))
    (define inner-result (if is-toehold
                             (toehold symbol)
                             symbol))
    (if is-complement
        (complement inner-result)
        inner-result)
    ))

(define (parse-domain-string input symbol-table)
  (filter
   (lambda (e) (not (null? e)))
   (map
    (lambda (input)
      (if (regexp-match (regexp "^\\?\\?$") input)
          (domain-hole)
          (parse-sequence-string input symbol-table)))
    (string-split input))))

(define (parse-upper-strand-string input symbol-table)
  (upper-strand (parse-domain-string (string-trim input (regexp "<|>")) symbol-table)))

(define (parse-lower-strand-string input symbol-table)
  (lower-strand (parse-domain-string (string-trim input (regexp "{|}")) symbol-table)))

(define (parse-gate-string input symbol-table)
  (let* ([components (string-split input (regexp "\\[|\\]") #:trim? #f)]
         [left-side (list-ref components 0)]
         [complex (list-ref components 1)]
         [right-side (list-ref components 2)])
    (gate
          (parse-upper-strand-string (car (or (regexp-match (regexp "<.*>") left-side) `(""))) symbol-table)
          (parse-lower-strand-string (car (or (regexp-match (regexp "{.*}") left-side) `(""))) symbol-table)
          (duplex-strand (parse-domain-string complex symbol-table))
          (parse-lower-strand-string (car (or (regexp-match (regexp "{.*}") right-side) `(""))) symbol-table)
          (parse-upper-strand-string (car (or (regexp-match (regexp "<.*>") right-side) `(""))) symbol-table)
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

