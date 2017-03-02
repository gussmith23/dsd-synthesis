#lang rosette/safe

; This file provides functions that manipulate the format of strands
; This includes reversing domains, taking the complement of domains,
; rotating species, and getting the canonical normal form of species.

(provide reverse-domain-list       ; Given a domain list, reverses it
         complement-of-domain      ; Given a domain, returns the complement of it
         complement-of-domain-list ; Given a domain list, returns a domain list with everything complemented
         rotate-species            ; Given a species, returns the 180 rotation
         normalize                 ; Given a species, returns the equivalent normal form
         reverse-species           ; Given a species or list of species, reverse it
         )


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

; Compares domains.
; Returns:
;  #f if they're equivalent
;  -1 if d1 is before d2
;   1 if d1 is after d2
(define (compare-domain d1 d2)
  (match* (d1 d2)
    [((complement a) (complement b)) (compare-domain a b)]
    [((complement _) (toehold _)) -1]
    [((complement _) _) -1]

    [((toehold a) (toehold b)) (compare-domain a b)]
    [((toehold _) (complement _)) 1]
    [((toehold _) _) -1]

    [(_ (toehold _)) 1]
    [(_ (complement _)) 1]

    [(a b) (cond [(< a b) -1]
                 [(> a b) 1]
                 [else #f])]))

; Compares domain lists.
; Returns:
;  #f if they're equivalent
;  -1 if dl1 is before dl2
;   1 if dl1 is after dl2
(define (compare-domain-list dl1 dl2)
  (cond [(> (length dl1) (length dl2)) -1]
        [(< (length dl1) (length dl2)) 1]
        [(eq? null dl1) #f]
        [else (let ([comp-result (compare-domain (car dl1) (car dl2))])
                (if comp-result
                    comp-result
                    (compare-domain-list (cdr dl1) (cdr dl2))))]))

; Compares gates.
; Returns:
;   0 if they're equivalent
;  -1 if g1 is before g2
;   1 if g1 is after g2
; Equivalent means in the sense of eq?, not normal forms
(define (compare-gates g1 g2)
  (match* (g1 g2)
    [((gate (upper-strand ur-1)
            (lower-strand lr-1)
            (duplex-strand d-1)
            (lower-strand ll-1)
            (upper-strand ul-1))
      (gate (upper-strand ur-2)
            (lower-strand lr-2)
            (duplex-strand d-2)
            (lower-strand ll-2)
            (upper-strand ul-2)))
     (or (compare-domain-list ur-1 ur-2)
         (compare-domain-list lr-1 lr-2)
         (compare-domain-list d-1 d-2)
         (compare-domain-list ll-1 ll-2)
         (compare-domain-list ul-1 ul-2)
         0) ; return 0 as default 'equals' signal
     ]))

; Makes it so that the left side of gate concatenations will always be a gate
; while the right side can be a gate or another gate concatenation
(define (normalize-gate-concatenation gc)
  (define (traverse g result)
    (match g
      [(gate: g1 g2) (traverse g2 (cons 'gate: (traverse g1 result)))]
      [(gate:: g1 g2) (traverse g2 (cons 'gate:: (traverse g1 result)))]
      [(gate _ _ _ _ _) (cons g result)]))
  (define infix-form (reverse (traverse gc null)))
  (define (parse-infix-form form)
    (match form
      [(list operand op other-operands ...)
       (cond [(eq? 'gate: op) (gate: operand (parse-infix-form other-operands))]
             [(eq? 'gate:: op) (gate:: operand (parse-infix-form other-operands))]
             )]
      [(list last-operand) last-operand]))
  (parse-infix-form infix-form))

; Given a gate concatenation gc in the form described by normalize-gate-concatenation,
; pushes all shared overhangs to the left.
(define (push-overhangs-left gc)
  (define (get-adjacent-gate gc)
    (define right-side
      (match gc
        [(gate: _ g2) g2]
        [(gate:: _ g2) g2]))
    (match right-side
      [(gate: g1 _) g1]
      [(gate:: g1 _) g1]
      [_ right-side])) ; else right-side must be a gate
  (match gc
    ; case of a gate lower-concatenated with...
    [(gate: (gate ul-1 ll-1 d-1 (lower-strand lr-d-1) ur-1) other-gc)
     (define new-other-gc (push-overhangs-left other-gc))
     (match new-other-gc
       ; another lower concatenation
       [(gate: (gate ul-2 (lower-strand ll-d-2) d-2 lr-2 ur-2) g2)
        (gate: (gate ul-1 ll-1 d-1 (lower-strand (append lr-d-1 ll-d-2)) ur-1)
               (gate: (gate ul-2 (lower-strand null) d-2 lr-2 ur-2)
                      g2))]

       ; another upper concatenation
       [(gate:: (gate ul-2 (lower-strand ll-d-2) d-2 lr-2 ur-2) g2)
        (gate: (gate ul-1 ll-1 d-1 (lower-strand (append lr-d-1 ll-d-2)) ur-1)
               (gate:: (gate ul-2 (lower-strand null) d-2 lr-2 ur-2)
                       g2))]

       ; another gate
       [(gate ul-2 (lower-strand ll-d-2) d-2 lr-2 ur-2)
        (gate: (gate ul-1 ll-1 d-1 (lower-strand (append lr-d-1 ll-d-2)) ur-1)
               (gate ul-2 (lower-strand null) d-2 lr-2 ur-2))])]
    ; case of gate upper-concatenated with...
    [(gate:: (gate ul-1 ll-1 d-1 lr-1 (upper-strand ur-d-1)) other-gc)
     (define new-other-gc (push-overhangs-left other-gc))
     (match new-other-gc
       ; anotoher lower concatenation
       [(gate: (gate (upper-strand ul-d-2) ll-2 d-2 lr-2 ur-2) g2)
        (gate:: (gate ul-1 ll-1 d-1 lr-1 (upper-strand (append ur-d-1 ul-d-2)))
                (gate: (gate (upper-strand null) ll-2 d-2 lr-2 ur-2)
                       g2))]

       ; another upper concatenation
       [(gate:: (gate (upper-strand ul-d-2) ll-2 d-2 lr-2 ur-2) g2)
        (gate:: (gate ul-1 ll-1 d-1 lr-1 (upper-strand (append ur-d-1 ul-d-2)))
                (gate:: (gate (upper-strand null) ll-2 d-2 lr-2 ur-2)
                        g2))]

       ; another gate
       [(gate (upper-strand ul-d-2) ll-2 d-2 lr-2 ur-2)
        (gate:: (gate ul-1 ll-1 d-1 lr-1 (upper-strand (append ur-d-1 ul-d-2)))
                (gate (upper-strand null) ll-2 d-2 lr-2 ur-2))])]

    ; case of just a gate
    [(gate _ _ _ _ _) gc]))

; Given a gate g, expands the duplex as much as possible by combining the left and right overhangs
; if domains next to the duplex are complements of each other. Returns the result.
(define (zip-up-duplex g)
  (define (get-common-prefix lower upper result)
    (define intermediate (match* (lower upper)
                     [((list lower-first lower-rest ...)
                       (list upper-first upper-rest ...))
                      (if (eq? upper-first
                               (complement-of-domain lower-first))
                          (get-common-prefix lower-rest upper-rest (cons upper-first result))
                          (list result lower upper))]
                     [(_ _) (list result lower upper)]))
    (match intermediate
      [(list prefix new-lower new-upper)
       (list (reverse prefix) new-lower new-upper)]))
  (match g
    [(gate (upper-strand ul)
           (lower-strand ll)
           (duplex-strand d)
           (lower-strand lr)
           (upper-strand ur))
     (define left-zip (get-common-prefix (reverse ll) (reverse ul) null))
     (define right-zip (get-common-prefix lr ur null))
     (match* (left-zip right-zip)
       [((list left-zip-result new-ll new-ul)
         (list right-zip-result new-lr new-ur))
        (gate (upper-strand (reverse new-ul))
              (lower-strand (reverse new-ll))
              (duplex-strand (append (reverse left-zip-result) d right-zip-result))
              (lower-strand new-lr)
              (upper-strand new-ur))])
     ]))

; Returns a new gate concatenation where all the gates in the given gate concatenation
; have their duplexes zipped up
(define (zip-up-all-gates gc)
  (match gc
    [(gate: g1 g2) (gate: (zip-up-all-gates g1) (zip-up-all-gates g2))]
    [(gate:: g1 g2) (gate:: (zip-up-all-gates g1) (zip-up-all-gates g2))]
    [(gate _ _ _ _ _) (zip-up-duplex gc)]))

; Returns the normal form of gates (including gate concatenations) without considering the rotation rule
;   - This means, the tree representing gate concatenations always have left children as non-concatenated gates
;   - All overhangs are pushed to the left
;   - And duplexes are as big as possible
(define (normalize-gate-without-rotation g)
  (define concatenations-normalized (normalize-gate-concatenation g))
  (define overhangs-normalized (push-overhangs-left concatenations-normalized))
  (zip-up-all-gates overhangs-normalized))

; Compares two gates that are in normal form (including gate concatenations!)
; Returns
;   0 if they're equivalent
;  -1 if g1 is before g2
;   1 if g1 is after g2
(define (compare-normal-gates g1 g2)
  (match* (g1 g2)
    [((gate: _ _) (gate:: _ _)) 1]
    [((gate: _ _) (gate _ _ _ _ _)) -1]
    [((gate: ga1 ga2) (gate: gb1 gb2))
     (define first-compare (compare-normal-gates ga1 gb1))
     (if (eq? first-compare 0)
         (compare-normal-gates ga2 gb2)
         first-compare)]
    [((gate:: _ _) (gate: _ _)) -1]
    [((gate:: _ _) (gate _ _ _ _ _)) -1]
    [((gate:: ga1 ga2) (gate:: gb1 gb2))
     (define first-compare (compare-normal-gates ga1 gb1))
     (if (eq? first-compare 0)
         (compare-normal-gates ga2 gb2)
         first-compare)]
    [((gate _ _ _ _ _) (gate:: _ _)) 1]
    [((gate _ _ _ _ _) (gate: _ _)) 1]
    [((gate _ _ _ _ _) (gate _ _ _ _ _))
     (compare-gates g1 g2)]))

(define (pick-normal-gate-form g)
  (let* ([g1 (normalize-gate-without-rotation g)]
         [g2 (normalize-gate-without-rotation (rotate-species g))]
         [compare-result (compare-normal-gates g1 g2)])
    (if (= compare-result 1)
        g2
        g1)))

; Takes a species and returns an equivalent canonical form
(define (normalize species)
  (for/all ([species species])
  (match species
    ; Normal form of strands are the upper strands
    [(upper-strand _) species]
    [(lower-strand _) (rotate-species species)]

    ; Normal forms of gates:
    [(gate _ _ _ _ _) (pick-normal-gate-form species)]
    [(gate: _ _) (pick-normal-gate-form species)]
    [(gate:: _ _) (pick-normal-gate-form species)]))
)

; reverse a list of species or a single species
(define (reverse-species species)
  (match species
    [ '() '() ]
    [ (? list? species) (map reverse-species species) ]
    [ (upper-strand domains) (upper-strand (reverse domains)) ]
    [ (lower-strand domains) (lower-strand (reverse domains)) ]
    [ (duplex-strand domains) (duplex-strand (reverse domains)) ]

    [ (gate lu ll s rl ru)
      (gate
       (reverse-species ru)
       (reverse-species rl)
       (reverse-species s)
       (reverse-species ll)
       (reverse-species lu)) ]

    [ (gate: g1 g2)
      (gate: (reverse-species g2) (reverse-species g1)) ]

    [ (gate:: g1 g2)
      (gate:: (reverse-species g2) (reverse-species g1)) ]
    ))

; Tests
(module+ test
  (require rackunit)
  (require "dna-syntax.rkt")
  (require "dna-string-parser.rkt")

  (define test-gate-1 (string->species "<L1>{L1b}[S1]{R1b}<R1>:<L2>{S L2b}[S2]{R2b}<R2>"))
  (define expected-output-1 (string->species "<R2b>{R2}[S2*]{L2}<L2b S R1b>::{R1}[S1*]{L1}<L1b>"))

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
   (normalize (normalize test-gate-1))
   expected-output-1
   "Test double normalization")

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
   (species->string (zip-up-duplex (string->species "<a b c>{d b* c*}[mid]{x* y* z*}<x y z>")))
   "<a>{d}[b c mid z y x]"
   "Test zip-up-duplex")
  
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
  (define arbitrary-gate-concatenation (gate: (gate-?? 3) (gate:: (gate-?? 3) (gate-?? 3))))
  (define verification-tests
    (test-suite "Verification tests for strand-manipulation.rkt"
                (printf "Running test 1...")
                (time (check-equal?
                 (verify (assert (eq? arbitrary-gate-concatenation
                                      (rotate-species (rotate-species arbitrary-gate-concatenation)))))
                 (unsat) ; verify can't find counter example
                 "Test double rotation is identity"))
                (printf " Done!\n")

                (printf "Running test 2...")
                (time (check-equal?
                 (verify (assert (eq? (normalize arbitrary-gate-concatenation)
                                      (normalize (rotate-species arbitrary-gate-concatenation)))))
                 (unsat)
                 "Test that normalization finds a normal form regardless of rotation for a concatenation of 3 gates with domains of size 3"))
                (printf " Done!\n")

                (printf "Running test 3...")
                (time (check-equal?
                 (verify (assert (eq? (normalize arbitrary-gate-concatenation)
                                      (normalize (normalize arbitrary-gate-concatenation)))))
                 (unsat)
                 "Test double normalization of arbitrary gate concatenation of 3 gates with domains of size 3"))
                (printf " Done!\n")
                ))

  (printf "Running verification-tests (Expensive tests!)\n")
  (require rackunit/text-ui)
  (run-tests verification-tests)
  (printf "Done running verification tests\n")
  )
