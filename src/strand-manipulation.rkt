#lang rosette/safe

; This file provides functions that manipulate the format of strands
; This includes reversing domains, taking the complement of domains,
; rotating species, and getting the canonical normal form of species.

(provide reverse-domain
         complement
         rotate-species
         normalize)

(require rosette/lib/match)

; Reverses a domain so the sequences are in reverse order
(define (reverse-domain domain)
  (reverse domain))

; Takes the complement of the domain so the result is every
; sequence that was marked complemented is no longer marked and vice-versa
(define (complement domain)
  (map (lambda (sequence)
         (match sequence
           [`(C ,rest ...) rest]
           [`(,desc ,id) `(C ,desc ,id)]
           )
         ) domain))

; Takes a species and returns the 180 rotation of it
(define (rotate-species species)
  (match species
    [`(: ,left-gate ,right-gate) (list `:: (rotate-species right-gate) (rotate-species left-gate))]
    [`(:: ,left-gate ,right-gate) (list `: (rotate-species right-gate) (rotate-species left-gate))]
    [`(gate (U (,ul ...)) (L (,ll ...)) (,middle ...) (L (,lr ...)) (U (,ur ...)))
     (list `gate
           `(U ,(reverse-domain lr))
           `(L ,(reverse-domain ur))
           (reverse-domain (complement middle))
           `(L ,(reverse-domain ul))
           `(U ,(reverse-domain ll)))]
    [`(U (,strand ...)) (list `L (reverse-domain strand))]
    [`(L (,strand ...)) (list `U (reverse-domain strand))]
    ))

; Takes a species and returns an equivalent canonical form
(define (normalize species)
  (match species
    ; Normal form of strands are the upper strands
    [`(L ,domain) (rotate-species species)]
    
    ; Normal forms of gates:
    ; TODO: Decide on canonical rotation first
    ; DONE: Then propogate all shared overhangs as far left as possible

    ; Below moves all bottom shared overhangs to the left
    [`(: (gate (U ,L1) (L ,L1_) ,S1 (L ,R1_) (U ,R1)) (gate (U ,L2) (L (,S ,L2_rest ...)) ,S2 (L ,R2_) (U ,R2)))
     (normalize
      `(: (gate (U ,L1) (L ,L1_) ,S1 (L ,(append R1_ `(,S))) (U ,R1)) (gate (U ,L2) (L ,L2_rest) ,S2 (L ,R2_) (U ,R2)))
      )]

    ; Below moves all top shared overhangs to the left
    [`(: (gate (U ,L1) (L ,L1_) ,S1 (L ,R1_) (U ,R1)) (gate (U (,S ,L2rest ...)) (L ,L2_) ,S2 (L ,R2_) (U ,R2)))
     (normalize
      `(: (gate (U ,L1) (L ,L1_) ,S1 (L ,R1_) (U ,(append R1 `(,S)))) (gate (U ,L2rest) (L ,L2_) ,S2 (L ,R2_) (U ,R2)))
      )]

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
   (species? test-gate-1)
   #t)
  (check-equal?
   (species? expected-output-1)
   #t)

  (check-equal?
   (normalize test-gate-1)
   expected-output-1)
  )