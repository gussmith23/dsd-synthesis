#lang rosette/safe

(require rosette/lib/match)
(require rosette/lib/synthax)

(require "dna-syntax.rkt")
(require "reduction-rules.rkt")

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

; reactions : (species, [species]) -> [species]
(define (reactions species species-list)
  (union (unary-reactions species) (binary-reactions species species-list)))

; unary-reactions : species -> [species]
(define (unary-reactions species)
  (map normalize
       (append
        (list species)
        (rule-ru species)
        (rule-rc species)
        ; TODO: rule-rm
        (rule-rd species)
        (rule-rga2 species)
        ; TODO: rule-rgu
        ; TODO: rule-rg
        ; TODO: rule-rv
        ; TODO: rule-rc
        ; TODO: rule-re
        )))

(define (binary-reactions s1 s2)

  (map normalize
       (append
        (list s1 s2)
        (rule-rb s1 s2)
        (rule-rga1 s1 s2)
        ; TODO: rule-rgb
        ; TODO: rule-rgl
        ; TODO: rule-rp
        )))

(module+ test
  (require rackunit)
  (require "dna-syntax.rkt")
  (require "dna-string-parser.rkt")

  ; define test inputs and outputs
  (define single-toehold (string->species "<a^>"))
  (define basic-RU-input (string->species "<l>{l'}[n^]{r'}<r>"))
  (define basic-RU-output (normalize (list (string->species "<l n^ r>")
                                           (string->species "{l' n^* r'}"))))

  ; basic parser tests which see if all parses are species
  (check-equal?
   (valid-dna-struct? basic-RU-input)
   #t)
  (check-equal?
   (andmap valid-dna-struct? basic-RU-output)
   #t)
  
  ; basic smoke test for unary reactions
  (check-equal?
   (unary-reactions single-toehold)
   (list single-toehold))

  (define (check-unary-reaction input output)
    (let [(actual (unary-reactions input))]
      (check-not-false
       (andmap
        (lambda (expected) member expected actual)
        output))))

  ; basic RU
  (check-unary-reaction
   basic-RU-input
   basic-RU-output)

  ; basic RC
  (check-unary-reaction
   (string->species "<l>{l'}[s]{n^* r'}<n^ r>")
   (normalize (list (string->species "<l>{l'}[s n^]{r'}<r>"))))

  ; TODO: still have to re-write RM rule
  ; basic RM
  ;(check-equal?
  ;(unary-reactions (string->species "<l>{l'}[s1]<s r2>:<l1>[s s2]{r'}<r>}"))
  ;(normalize (list (string->species "<l>{l'}[s1 s]<r2>:<l1 s>[s2]{r'}<r>}"))))
 )

(define (normalize a) a)

; placeholders
(define (products reacts) '())

