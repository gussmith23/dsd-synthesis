#lang rosette/safe

; This file provides functions that manipulate the format of strands
; This includes reversing domains, taking the complement of domains,
; rotating species, and getting the canonical normal form of species.

(provide reverse-domain-list       ; Given a domain list, reverses it
         complement-of-domain      ; Given a domain, returns the complement of it
         complement-of-domain-list ; Given a domain list, returns a domain list with everything complemented
         rotate-species            ; Given a species, returns the 180 rotation
         normalize)                ; Given a species, returns the equivalent normal form

(require rosette/lib/match
         "dna-syntax.rkt")

(define (reverse-domain-list domain-list)
  (reverse domain-list))

(define (complement-of-domain domain)
  (match domain
    [(complement inner) inner]
    [_ (complement domain)]))

(define (complement-of-domain-list domain-list)
  (map complement-of-domain domain-list))

; Takes a species and returns the 180 rotation of it
(define (rotate-species species)
  (match species
    [(gate: left-gate right-gate) (gate:: (rotate-species right-gate) (rotate-species left-gate))]
    [(gate:: left-gate right-gate) (gate: (rotate-species right-gate) (rotate-species left-gate))]
    [(gate (upper-strand ul)
           (lower-strand ll)
           (duplex-strand d)
           (lower-strand lr)
           (upper-strand ur))
     (gate (upper-strand (reverse-domain-list lr))
           (lower-strand (reverse-domain-list ur))
           (duplex-strand (reverse-domain-list (complement-of-domain-list d)))
           (lower-strand (reverse-domain-list ul))
           (upper-strand (reverse-domain-list ll)))]
    [(upper-strand strand) (lower-strand (reverse-domain-list strand))]
    [(lower-strand strand) (upper-strand (reverse-domain-list strand))]
    ))

; Takes a species and returns an equivalent canonical form
(define (normalize species)
  (match species
    ; Normal form of strands are the upper strands
    [(lower-strand _) (rotate-species species)]

    ; Normal forms of gates:
    ; TODO: Decide on canonical rotation first
    ; DONE: Then propogate all shared overhangs as far left as possible

    ; Below moves all bottom shared overhangs to the left
    [(gate: (gate (upper-strand L1) (lower-strand L1_) (duplex-strand D1) (lower-strand R1_) (upper-strand R1))
            (gate (upper-strand L2) (lower-strand (list h t ...)) (duplex-strand D2) (lower-strand R2_) (upper-strand R2)))
     (normalize
      (gate: (gate (upper-strand L1) (lower-strand L1_) (duplex-strand D1) (lower-strand (append R1_ (cons h t))) (upper-strand R1))
             (gate (upper-strand L2) (lower-strand null) (duplex-strand D2) (lower-strand R2_) (upper-strand R2))))]

    ; Below moves all top shared overhangs to the left
    [(gate: (gate (upper-strand L1) (lower-strand L1_) (duplex-strand D1) (lower-strand R1_) (upper-strand R1))
            (gate (upper-strand (list h t ...)) (lower-strand L2_) (duplex-strand D2) (lower-strand R2_) (upper-strand R2)))
     (normalize
      (gate: (gate (upper-strand L1) (lower-strand L1_) (duplex-strand D1) (lower-strand R1_) (upper-strand (append R1 (cons h t))))
             (gate (upper-strand null) (lower-strand L2_) (duplex-strand D2) (lower-strand R2_) (upper-strand R2))))]

    ; If no more transformations can be applied, the species is in normal form
    [_ species]
    ))

; Tests
(module+ test
  (require rackunit)
  (require "dna-syntax.rkt")
  (require "dna-string-parser.rkt")

  (define test-gate-1 (string->species "<L1>{L1b}[S1]{R1b}<R1>:<L2>{S L2b}[S2]{R2b}<R2>"))
  (define expected-output-1 (string->species "<L1>{L1b}[S1]{R1b S L2b}<R1 L2>:[S2]{R2b}<R2>"))

  (check-equal?
   (valid-dna-struct? test-gate-1)
   #t
   "Test gate is a valid gate")

  (check-equal?
   (valid-dna-struct? expected-output-1)
   #t
   "Expected output gate is a valid gate")

  (check-equal?
   (normalize test-gate-1)
   expected-output-1
   "Test normalization")

  (check-equal?
   (reverse-domain-list (list (complement 0) 1 (toehold 2) (complement (toehold 3))))
   (list (complement (toehold 3)) (toehold 2) 1 (complement 0))
   "Test domain reversal")

  (check-equal?
   (complement-of-domain (complement 0))
   0
   "Test complementing a complement")

  (check-equal?
   (complement-of-domain 0)
   (complement 0) "Test complementing a lone id")
  
  (check-equal?
   (complement-of-domain-list (list (complement 0) 1 (toehold 2) (complement (toehold 3))))
   (list 0 (complement 1) (complement (toehold 2)) (toehold 3))
   "Test complementing a domain list")

  (check-equal?
   (rotate-species (string->species "<A B C>"))
   (string->species "{C B A}")
   "Test rotation of upper strand")

  (check-equal?
   (rotate-species (string->species "{A B C}"))
   (string->species "<C B A>")
   "Test rotation of lower strand")

  ; Expensive synthesis tests
  (define arbitrary-gate (gate-?? 3))

  (check-equal?
   (verify (assert (eq? arbitrary-gate
                        (rotate-species (rotate-species arbitrary-gate)))))
   (unsat) ; verify can't find counter example
   "Test double rotation is identity")
  
  (check-equal?
   (verify (assert (eq? (normalize arbitrary-gate)
                        (normalize (normalize arbitrary-gate)))))
   (unsat) ; verify can't find a counter example
   "Test that the normal form of normal form is the same")
  )
