#lang rosette/safe

(require rosette/lib/match)

(provide
 ; domains are toeholds, complements, or ids ('integer?'s)
 (struct-out toehold)       ; toehold struct       - takes an id
 (struct-out complement)    ; complement struct    - takes a id or toehold
 (struct-out upper-strand)  ; upper-strand struct  - takes a list of domains
 (struct-out lower-strand)  ; lower-strand struct  - takes a list of domains
 (struct-out duplex-strand) ; duplex-strand struct - takes a list of domains
 (struct-out gate)          ; gate struct          - takes an upper-strand, lower-strand, duplex, lower-strand, and upper-strand
 (struct-out gate:)         ; gate: struct         - takes two gates
 (struct-out gate::)        ; gate:: struct        - takes two gates
 valid-dna-struct? ; returns #t iff the input is a valid dna struct, #f otherwise
                   ;  a valid dna struct is a dna struct (toehold, complement,
                   ;   upper-strand, lower-strand, duplex-strand, gate, gate:, gate::)
                   ;  and the dna struct is constructed only from valid inputs as described above
)

; Checking for equality between ids are all we need to implement
; DNA strand manipulation rules and integers support equality checking
; ids that are not equal represent distinct dna strands
; that won't interact with eachother.

; ids are considered 'domain's
; toehold and complement are also 'domain's
(struct toehold (id) #:transparent)               ; Represents a toehold as described by this id
(struct complement (id-or-toehold) #:transparent) ; Represents the complement of an id or toehold

; domain-list is a list of domains which can be
;   - ids
;   - toeholds (of ids)
;   - complements (of ids or toeholds of ids)
(struct upper-strand (domain-list) #:transparent)
(struct lower-strand (domain-list) #:transparent)
(struct duplex-strand (domain-list) #:transparent)

; gate, gate: and gate:: are all gates
(struct gate (left-upper  ; an upper-strand
              left-lower  ; a lower-strand
              duplex      ; a duplex
              right-lower ; a lower-strand
              right-upper ; an upper-strand
              ) #:transparent)
(struct gate: (g1 g2) #:transparent) ; g1 and g2 are gates
(struct gate:: (g1 g2) #:transparent) ; g1 and g2 are gates

(define (gate-struct? g)
  (or (gate? g)
      (gate:? g)
      (gate::? g)))

(define (domain? dna-struct)
  (or (toehold? dna-struct)
      (complement? dna-struct)
      (integer? dna-struct)))

(define (domain-list? domain-list)
  (and (list? domain-list) (andmap domain? domain-list)))

(define (valid-dna-struct? dna-struct)
  (match dna-struct
    [(toehold id) (integer? id)]
    [(complement id-or-toehold)
     (or
      (integer? id-or-toehold)
      (and (toehold? id-or-toehold) (valid-dna-struct? id-or-toehold)))]
    [(upper-strand domain-list) (domain-list? domain-list)]
    [(lower-strand domain-list) (domain-list? domain-list)]
    [(duplex-strand domain-list) (domain-list? domain-list)]
    [(gate lu ll d rl ru)
     (and
      (upper-strand? lu) (valid-dna-struct? lu)
      (lower-strand? ll) (valid-dna-struct? ll)
      (duplex-strand? d) (valid-dna-struct? d)
      (lower-strand? rl) (valid-dna-struct? rl)
      (upper-strand? ru) (valid-dna-struct? ru))]
    [(gate: g1 g2)
     (and
      (gate-struct? g1) (valid-dna-struct? g1)
      (gate-struct? g2) (valid-dna-struct? g2))]
    [(gate:: g1 g2)
     (and
      (gate-struct? g1) (valid-dna-struct? g1)
      (gate-struct? g2) (valid-dna-struct? g2))]
    [_ #f]))