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

; <id>       := NAME
;
; <sequence> := (S <id>)
;             | (T <id>)
;             | (C S <id>)
;             | (C T <id>)
;
; <domain>   := (<sequence>*)
;
; <upper>    := (U domain)
; <lower>    := (L domain)
;
; <strand>   := upper
;             | lower
;
; <gate>     := (gate <upper> <lower> <domain> <lower> <upper>)
;
; <species>  := <strand>
;             | <gate>
;
; <system>   := (<species>+)

; reactions : (species, [species]) -> [species]
(define (reactions species species-list)
  (union (unary-reactions species) (binary-reactions species species-list)))

; unary-reactions : species -> [species]
(define (unary-reactions species)
  (match species
    ; Rule RU
    [ `(gate (U ,lu) (L ,ll) (T ,n) (L ,rl) (U ,ru))
      (normalize (list `(U ,lu (T ,n) ,ru) `(L ,ll (C (T ,n)) ,rl))) ]

    ; Rule RGA2
    [ `(: ,g (gate (U ,l) (L ϵ) ,s (L ,rl) (U ,ru)))
      (match (unary-reactions g)
        [ (list `(U ,s2) `(L ,s1)) (normalize (list `(gate (U ,l) (L ,s1) ,s (L ,rl) (U ,ru)) `(U ,s2))) ]
        [ _ (list species) ] )]
     
    [ _ (list species) ]
  ))

(module+ test
  (require rackunit)
  ; basic smoke test for unary reactions
  (check-equal? (list `(T a)) (unary-reactions `(T a)))
  ; basic RU
  (check-equal?
   (unary-reactions `(gate (U l-prime) (L l) (T n) (L r) (U r-prime)))
   (list `(U l-prime (T n) r-prime) `(L l (C (T n)) r))) 
 )

(define (normalize a) a)

(define (binary-reactions a b) '())

; placeholders
(define (products reacts) '())
