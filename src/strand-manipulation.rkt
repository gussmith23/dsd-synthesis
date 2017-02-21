#lang rosette/safe

; This file provides functions that manipulate the format of strands
; This includes reversing domains, taking the complement of domains,
; rotating species, and getting the canonical normal form of species.

(provide reverse-domain-list
         complement-of-domain-list
         rotate-species
         normalize)

(require rosette/lib/match
         "dna-syntax.rkt")

(define (reverse-domain-list domain-list)
  (reverse domain-list))

(define (complement-of-domain-list domain-list)
  (map (lambda (sequence)
         (match sequence
           [(complement rest) rest]
           [_ (complement sequence)]))
       domain-list))

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
            (gate (upper-strand L2) (lower-strand (list shared-overhang L2_rest ...)) (duplex-strand D2) (lower-strand R2_) (upper-strand R2)))
     (normalize
      (gate: (gate (upper-strand L1) (lower-strand L1_) (duplex-strand D1) (lower-strand (append R1_ (list shared-overhang))) (upper-strand R1))
             (gate (upper-strand L2) (lower-strand L2_rest) (duplex-strand D2) (lower-strand R2_) (upper-strand R2))))]

    ; Below moves all top shared overhangs to the left
    [(gate: (gate (upper-strand L1) (lower-strand L1_) (duplex-strand D1) (lower-strand R1_) (upper-strand R1))
            (gate (upper-strand (list shared-overhang L2-rest ...)) (lower-strand L2_) (duplex-strand D2) (lower-strand R2_) (upper-strand R2)))
     (normalize
      (gate: (gate (upper-strand L1) (lower-strand L1_) (duplex-strand D1) (lower-strand R1_) (upper-strand (append R1 (list shared-overhang))))
             (gate (upper-strand L2-rest) (lower-strand L2_) (duplex-strand D2) (lower-strand R2_) (upper-strand R2))))]

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
   #t)
  (check-equal?
   (valid-dna-struct? expected-output-1)
   #t)

  (check-equal?
   (normalize test-gate-1)
   expected-output-1)
  )