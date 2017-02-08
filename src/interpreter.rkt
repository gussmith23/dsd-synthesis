#lang rosette/safe

(require rosette/lib/match)

(struct state (Is Os) #:transparent)
(define empty-state (state '() '()))

(define (∪ l1 l2) (remove-duplicates (append l1 l2)))

; Table 7 from supplementary material (saturating mode)
(define (⊕ I T)
  (let ([Is (state-Is T)]
        [Os (state-Os T)])
    (cond
      [ (list? I) (foldr ⊕ T I) ]
      [ (member I Is) T ]
      [ else
        (let ([Os′ (reactions I Is)])
          (⊕ (products Os′) (state (∪ Is (list I)) (∪ Os Os′))))])))

; placeholders
(define (reactions I Is) '())
(define (products reacts) '())