#lang rosette/safe

(require rosette/lib/match)
(require rosette/lib/synthax)

(require "dna-syntax.rkt")
(require "reduction-rules.rkt")
(require "strand-manipulation.rkt")

(provide compile
         initial-state
         (struct-out state)
         (struct-out reaction))

(struct reaction (inputs output) #:transparent)
(struct state (species reactions unrolls) #:transparent)
(define (initial-state k) (state '() '() k))

(define (union l1 l2) (remove-duplicates (append l1 l2)))

; compile: ([species]|species, state) -> state
(define (compile species current-state)
  (let ([current-species (state-species current-state)]
        [current-reactions (state-reactions current-state)])
    (cond
      ; we've run out of time.
      [ (= (state-unrolls current-state) 0) current-state ]

      ; no species to consider
      [ (null? species) current-state ]

      ; process a list of species
      [ (list? species) (foldr compile current-state species) ]

      ; fixed point
      [ (member species current-species) current-state ]

      [ else
        (let*
            ; find reactions between species and all species in state
            ([new-reactions (reactions species current-species)]
             ; new species are prodcts of those reactions
             [new-species (products new-reactions)])

          (compile
           ; recurse on the list of new species
           new-species
           ; and a new state that includes the new reactions
           (state
            (union current-species (list species))
            (union current-reactions new-reactions)
            (- (state-unrolls current-state) 1)))) ])))

; products : [reaction] -> [species]
(define (products reaction-list)
  (map reaction-output reaction-list))

; reactions : (species, [species]) -> [reactions]
(define (reactions species species-list)
  (binary-reactions species species-list))
  ;(append
  ; (unary-reactions species)
  ; (binary-reactions species species-list)))

; unary-reactions : species -> [reactions]
(define (unary-reactions species)
  (define (unary-rules species)
    (append
     (rule-ru species)
     (rule-rc species)
     (rule-rd-rm species)
     (rule-rga2 species)
     (rule-rgu species)
     ; TODO: rule-rg
     ; TODO: rule-rc
     ))
  ; rules are invariant to reversal
  (define (rule-rv species)
    (append
     (unary-rules species)
     (reverse-species
      (unary-rules (reverse-species species)))))

  (map
   (lambda (product) (reaction (list species) product))
   (rule-rv species)))

; binary-reactions : (species, [species]) -> [reactions]
(define (binary-reactions s1 species-list)

  (define (binary-rules s1 s2)
    (append
     ;(rule-rb s1 s2)
     ;(rule-rga1 s1 s2)
     ;(rule-rgb s1 s2)
     (rule-rgl s1 s2)
     (rule-rp s1 s2)
     ))

  ; binary rules are coummutative
  (define (rule-re s1 s2)
    (append
     (binary-rules s1 s2)
     (binary-rules s2 s1)))

  ; rules are invariant to reversal
  (define (rule-rv s1 s2)
    (append
     (rule-re s1 s2)
     (reverse-species
      (rule-re (reverse-species s1)
               (reverse-species s2)))))

  (apply append
         (map
          (lambda (s2)
            (map
             (lambda (product) (reaction (list s1 s2) product))
             (rule-rv s1 s2)))
          species-list)))

; tests are commented out right now because of a major rework
#;(module+ test
  (require rackunit)
  (require "dna-syntax.rkt")
  (require "dna-string-parser.rkt")
  (require "test.rkt")

  ; define test inputs and outputs
  (define single-toehold (string->species "<a^>"))
  (define basic-RU-input (string->species "<l>{l'}[n^]{r'}<r>"))
  (define basic-RU-output (list (string->species "<l n^ r>")
                                (string->species "{l' n^* r'}")))

  ; basic parser tests which see if all parses are species
  (check-equal?
   (valid-dna-struct? basic-RU-input)
   #t)
  (check-equal?
   (andmap valid-dna-struct? basic-RU-output)
   #t)

  ; basic smoke test for unary reactions
  (test-exact
   (unary-reactions single-toehold)
   '())

  (define (check-unary-reaction input output)
    (let [(actual (products (unary-reactions input)))]
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
   (list (string->species "<l>{l'}[s n^]{r'}<r>")))

  ; TODO: still have to re-write RM rule
  ; basic RM
  (check-unary-reaction
   (string->species "<l>{l'}[s1]<s r2>:<l1>[s s2]{r'}<r>}")
   (list (string->species "<l>{l'}[s1 s]<r2>:<l1 s>[s2]{r'}<r>}")))

  (define and-test-system
    (list
     (upper-strand (list (toehold 1) 2))
     (upper-strand (list 3 (toehold 4)))
     (gate (upper-strand '())
           (lower-strand (list (complement (toehold 1))))
           (duplex-strand (list 2 3))
           (lower-strand (list (complement (toehold 4))))
           (upper-strand '()))))

  (check-not-false
   (member
    (upper-strand (list 2 3))
    (state-species (compile and-test-system (initial-state -1)))))

  )