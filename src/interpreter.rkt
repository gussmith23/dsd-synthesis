#lang rosette/safe

(require rosette/lib/match)

(require "dna-syntax.rkt")
(require "reduction-rules.rkt")
(require "strand-manipulation.rkt")

(provide
 enumerate-reactions
 parents
 reaction-requires
 (struct-out reaction))

(define (concat-map f xs) (apply append (map f xs)))

; enumerate all reactions between initial species, and between
; the initial species and the products of those reactions
(define (enumerate-reactions species)
  (let* ([gates (filter gate? species)]
         [strands (filter upper-strand? species)]

         ; compute reactions between initial strands and initial gates
         [strand-gate-reactions
          (concat-map
           (lambda (strand)
             (concat-map
              (lambda (gate)
                (binary-reactions strand gate))
              gates))
           strands)]

         ; compute reactions between initial strands and new gates
         [strand-newgate-reactions
          (concat-map
           (lambda (strand)
             (concat-map
              (lambda (first-rxn)
                (map
                 (lambda (second-rxn)
                   (reaction
                    strand first-rxn
                    (reaction-strand-out second-rxn)
                    (reaction-gate-out second-rxn)))
                 (binary-reactions strand (reaction-gate-out first-rxn))))
              strand-gate-reactions))
           strands)]

         ; compute reactions between new strands and initial gates
         [newstrand-gate-reactions
          (concat-map
           (lambda (gate)
             (concat-map
              (lambda (first-rxn)
                (map
                 (lambda (second-rxn)
                   (reaction
                    first-rxn gate
                    (reaction-strand-out second-rxn)
                    (reaction-gate-out second-rxn)))
                 (binary-reactions (reaction-strand-out first-rxn) gate)))
              strand-gate-reactions))
           gates)]
         )

    (append
     strand-gate-reactions
     strand-newgate-reactions
     newstrand-gate-reactions)))

; apply the rule to the reverse of s1 and s2, then reverse the results
(define (reverse-rule rule s1 s2)
  (match (rule (reverse-species s1) (reverse-species s2))
    [ (reaction _ _ out1 out2)
      (reaction
       s1 s2
       (reverse-species out1)
       (reverse-species out2)) ]))

; binary-reactions : (strand, gate) -> [reaction, reaction]
(define (binary-reactions strand gate)
  (cond
    [ (or (null? strand) (null? gate))
      (list
       (reaction strand gate '() '())
       (reaction strand gate '() '())) ]

    [ (gate? gate)
      (list
       (rule-rp strand gate)
       (reverse-rule rule-rp strand gate)) ]

    [ (gate:? gate)
      (list
       (rule-rgl strand gate)
       (reverse-rule rule-rgl strand gate)) ]))

; given a strand and a list of reactions, filter them
; by the reactions that lead to the strand
(define (parents strand reactions)
  (map
   (lambda (rxn)
     (if (equal? (reaction-strand-out rxn) strand) rxn '()))
   reactions))

; this is true iff rxn requires strand, either directly or indirectly
(define (reaction-requires rxn strand)
  (and
   (reaction? rxn)
   (or (equal? (reaction-strand-in rxn) strand)
       (reaction-requires (reaction-gate-in rxn) strand)
       (reaction-requires (reaction-strand-in rxn) strand))))
