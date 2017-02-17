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

; reactions : (species, [species]) -> [species]
(define (reactions species species-list)
  (union (unary-reactions species) (binary-reactions species species-list)))

; unary-reactions : species -> [species]
(define (unary-reactions species)
  (match species
    ; Rule RU
    [ `(gate (U (,lu)) (L (,ll)) ((T ,n)) (L (,rl)) (U (,ru)))
      (normalize (list `(U (,lu (T ,n) ,ru)) `(L (,ll (C (T ,n)) ,rl)))) ]

    ; Rule RC
    [ `(gate (U ,lu) (L ,ll) (,s) (L ( (C (T ,n)) ,rl) ) (U ((T ,n) ,ru)))
      (normalize (list `(gate (U ,lu) (L ,ll) (,s (T ,n)) (L (,rl)) (U (,ru)) ) )) ]

    ; Rule RGA2
    [ `(: ,g (gate (U ,l) (L ϵ) ,s (L ,rl) (U ,ru)))
      (match (unary-reactions g)
        [ (list `(U ,s2) `(L ,s1)) (normalize (list `(gate (U ,l) (L ,s1) ,s (L ,rl) (U ,ru)) `(U ,s2))) ]
        [ _ (list species) ] )]
     
    [ _ (list species) ]
  ))


(struct domain (id) #:transparent)
(struct toehold (id) #:transparent)
(struct complement (domain) #:transparent)
(struct upper-strand (domain-list) #:transparent)
(struct lower-strand (domain-list) #:transparent)
(struct duplex-strand (domain-list) #:transparent)
(struct gate (left-upper left-lower duplex right-lower right-upper) #:transparent)
(struct gate: (g1 g2) #:transparent)
(struct gate:: (g1 g2) #:transparent)

(define (toe-search s1 s2) (toe-search-aux s1 s2 '() '()))

(define (toe-search-aux s1 s2 acc1 acc2)
  (match* (s1 s2)

    ; two domain lists
    [ ((cons d1 rest-s1) (cons d2 rest-s2))
      (match* (d1 d2)

        ; heads are toeholds
        [ ((toehold a) (complement (toehold b)))

          (if
           (equal? a b)
           ; heads are matching toeholds
           (list
            (list (reverse acc1) (toehold a) rest-s1)
            (list (reverse acc2) (complement (toehold a)) rest-s2))
           ; not matching toeholds
           (toe-search-aux s1 rest-s2 acc1 (cons d2 acc2))) ]

        ; search through second list
        [ (_ d2)
          (toe-search-aux s1 rest-s2 acc1 (cons d2 acc2)) ])]

    ; end of second list
    [ ((cons d1 rest-s1) '())
      ; restart through second list with next entry in first list
      (toe-search-aux rest-s1 (reverse acc2) (cons d1 acc1) '()) ]

    ; end of first list -- nothing found
    [ ( '() _ ) '() ]))


(define (binary-reactions s1 s2)
  (match* (s1 s2)
    ; rule rb
    [ ((upper-strand upper-domains) (lower-strand lower-domains))
      (match (toe-search upper-domains lower-domains)
        [ '() (list s1 s2) ]
        [ (list (list lu (toehold n) ru) (list ll _ rl))
          (list (gate (upper-strand lu) (lower-strand ll) (duplex-strand (list (toehold n))) (lower-strand rl) (upper-strand ru))) ] )]

    ;rule rga1
    [ ((gate lu s1 s rl ru) (upper-strand s2))

      (match (binary-reactions (upper-strand s2) s1)
        [ (list result)
          (if (gate? result)
              (list (gate: result (gate lu (lower-strand '()) s rl ru)))
              result) ])]

    [ (s1 s2) (list s1 s2) ]

    ))

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
   (species? basic-RU-input)
   #t)
  (check-equal?
   (andmap species? basic-RU-output)
   #t)
  
  ; basic smoke test for unary reactions
  (check-equal?
   (unary-reactions single-toehold)
   (list single-toehold))
  ; basic RU
  (check-equal?
   (unary-reactions basic-RU-input)
   basic-RU-output)
  ; basic RC
  (check-equal?
   (unary-reactions (string->species "<l>{l'}[s]{n^* r'}<n^ r>"))
   (normalize (list (string->species "<l>{l'}[s n^]{r'}<r>"))))
 )

(define (normalize a) a)


; placeholders
(define (products reacts) '())

