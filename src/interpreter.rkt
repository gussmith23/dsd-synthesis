#lang rosette/safe

(require rosette/lib/match)

(struct state (Is Os) #:transparent)
(define empty-state (state '() '()))

(define (union l1 l2) (remove-duplicates (append l1 l2)))

; Table 7 from supplementary material (saturating mode)
; compile: ([species]|species, state) -> state
(define (compile I T)
  (let ([Is (state-Is T)]
        [Os (state-Os T)])
    (cond
      [ (list? I) (foldr compile T I) ]
      [ (member I Is) T ]
      [ else
        (let ([Os-prime (reactions I Is)])
          (compile (products Os-prime) (state (union Is (list I)) (union Os Os-prime))))])))

; -- upper case or parens for terminals
; domain  := S
;          | domain S
;          | epsilon
;
; strand  := U domain
;          | L domain
;
; gate    := strand strand domain strand strand
;          | (:)  gate gate
;          | (::) gate gate
;
; species := strand | gate
;
; system  := species | (|) system system

; placeholders
(define (reactions I Is) '())
(define (products reacts) '())