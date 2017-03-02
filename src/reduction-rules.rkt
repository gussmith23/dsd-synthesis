#lang rosette/safe

(require rosette/lib/match)
(require "dna-syntax.rkt")

(provide
 ; unary rules:
 rule-ru
 rule-rc
 rule-rd-rm
 rule-rga2
 rule-rgu
 ; TODO: rule-rg
 ; TODO: rule-rv
 ; TODO: rule-rc
 ; TODO: rule-re

 ; binary rules:
 rule-rb
 ; TODO: rule-rp
 rule-rga1
 rule-rgb
 ; TODO: rule-rgl
 )

; Takes two lists of domains and locates the first pair of matching toeholds.
; For instance, given (a b c^ d) and (e c* f g h), this function produces (((a b) (c^) (d)) ((e) (c*) (f g h)))
; That's a list of lists of lists: the outer list contains two lists, each of which contains three lists, which
; are split around the toehold.
(define (toe-search s1 s2) (toe-search-aux s1 s2 '() '()))

; Helper function for toe-search. Do not call directly; call toe-search instead.
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

(define (rule-ru species)
  (match species
    [ (gate
       (upper-strand lu)
       (lower-strand ll)
       (duplex-strand s)
       (lower-strand rl)
       (upper-strand ru))

      (match s
        [ (list (toehold n))

          (list
           (upper-strand (append lu (list (toehold n)) ru))
           (lower-strand (append ll (list (complement (toehold n))) rl))) ]

        [ _ '() ] )]

    [ _ '() ] ))

(define (rule-rgu species)
  (match species
    [ (gate: left right)
      (match (rule-rd-rm left)
        [ '() '() ]

        ; migration
        [ (list new-left) (list (gate: new-left right)) ]

        ; displacement
        [ (list strand new-left) (list strand (gate: new-left right)) ]) ]

    [ _ '() ]))

(define (rule-rga2 species)
  (match species
    [ (gate:
       g1
       (gate
        (upper-strand l)
        (lower-strand empty)
        (duplex-strand s)
        (lower-strand r-prime)
        (upper-strand r)))

      (if (not (null? empty))
          '()

          (match (rule-ru g1)
            [(list
              (upper-strand s2)
              (lower-strand s1))

             (list
              (gate
               (upper-strand l)
               (lower-strand s1)
               (duplex-strand s)
               (lower-strand r-prime)
               (upper-strand r))

              (upper-strand s2)) ]

             [_ '() ] )) ]

    [ _ '() ]))

(define (rule-rc species)
  (match species
    ; if the species is a gate...
     [ (gate
        (upper-strand lu)
        (lower-strand ll)
        (duplex-strand s)
        (lower-strand rl)
        (upper-strand ru))

       (match* (ru rl)
         ; and the right upper and right lower are not null...
         [ ((cons t ru-rest) (cons ct rl-rest))

           (match* (t ct)

             ; and first elements of each are potentially complementary toeholds...
             [ ((toehold a) (complement (toehold b)))

               ; and the toeholds are equal...
               (if (= a b)

                   ; then the toeholds bind.
                   (list
                    (gate
                     (upper-strand lu)
                     (lower-strand ll)
                     (duplex-strand (append s (list t)))
                     (lower-strand rl-rest)
                     (upper-strand ru-rest)))

                   ; toeholds aren't equal
                   '() )]

             ; not potentially complementary
             [ (_ _) '() ] )]

         ; right upper or right lower are null
         [ (_ _) '() ] )]

    ; not a gate
    [ _ '() ] ))




(define (rule-rd-rm species)
  (match species
    [ (gate:
       (gate
        (upper-strand l)
        (lower-strand l-prime)
        (duplex-strand s1)
        (lower-strand empty-1)
        (upper-strand sr-concat))
       (gate
        (upper-strand l2)
        (lower-strand empty-2)
        (duplex-strand s)
        (lower-strand r-prime)
        (upper-strand r2)))

      (if
       (and (null? empty-1)
            (null? empty-2)
            (not (null? s))
            (not (null? sr-concat))
            (equal? (car sr-concat) (car s)))

       (if
        (null? (cdr s))

        ; displacement
        (list
         (upper-strand (append l2 s r2))
         (gate
          (upper-strand l)
          (lower-strand l-prime)
          (duplex-strand (append s1 (list (car s))))
          (lower-strand r-prime)
          (upper-strand (cdr sr-concat))))

        ; migration
        (list
         (gate:
          (gate
           (upper-strand l)
           (lower-strand l-prime)
           (duplex-strand (append s1 (list (car s))))
           (lower-strand '())
           (upper-strand (cdr sr-concat)))
          (gate
           (upper-strand (append l2 (list (car s))))
           (lower-strand '())
           (duplex-strand (cdr s))
           (lower-strand r-prime)
           (upper-strand r2)))))


       '()) ]

    [ _ '() ] ))


(define (rule-rb s1 s2)
  (match* (s1 s2)

    [ ((upper-strand upper-domains) (lower-strand lower-domains))

      (match (toe-search upper-domains lower-domains)

        [ '() '() ]

        [ (list
           (list lu (toehold n) ru)
           (list ll _ rl))

          (list
           (gate
            (upper-strand lu)
            (lower-strand ll)
            (duplex-strand (list (toehold n)))
            (lower-strand rl)
            (upper-strand ru))) ] )]

    [ (_ _) '() ] ))

(define (rule-rgb s1 s2)
  (match* (s1 s2)
    [ ((gate: (gate: left mid) right) s2)

      (let* ([maybe-left  (rule-rga1 left  s2)]
             [maybe-mid   (rule-rga1 mid   s2)]
             [maybe-right (rule-rga1 right s2)]
             [new-left    (if (null? maybe-left)  left  (car maybe-left) )]
             [new-mid     (if (null? maybe-mid)   mid   (car maybe-mid)  )]
             [new-right   (if (null? maybe-right) right (car maybe-right))])

        (if (and (null? maybe-left) (null? maybe-mid) (null? maybe-right))
            '()
            (list (gate: (gate: (new-left new-mid) new-right))))) ]

    [ ((gate: left right) s2)

      (let* ([maybe-left  (rule-rga1 left  s2)]
             [maybe-right (rule-rga1 right s2)]
             [new-left    (if (null? maybe-left)  left  (car maybe-left))]
             [new-right   (if (null? maybe-right) right (car maybe-right))])

        (if (and (null? maybe-left) (null? maybe-right))
            '()
            (list (gate: new-left new-right)))) ]

    [ (_ _) '() ]))


(define (rule-rga1 s1 s2)
  (match* (s1 s2)
    [ ((gate lu s1 s rl ru) (upper-strand s2))

      (let ([ result (rule-rb (upper-strand s2) s1) ])
        (if (and (not (null? result)) (gate? (car result)))
            (list
             (gate:
              (car result)
              (gate lu (lower-strand '()) s rl ru)))
            '())) ]

    [ (_ _) '() ] ))

(module+ test
  (require rackunit)
  (require "dna-string-parser.rkt")

  (define (solver-check checker-func args)
    (define formula (apply checker-func args))
    (define cex (solve (assert (not formula))))
    (if (sat? cex) (map (λ (x) (display (species->string (evaluate x cex)))) args)
    (check-equal? cex (unsat)))
    (clear-asserts!))

  (define upper (upper-strand (domain-cat-?? 3)))
  (define lower (lower-strand (domain-cat-?? 3)))

  (define (in y xs)
    (match xs
      [ (cons x xs)
        (if (equal? x y) #t (in y xs)) ]
      [ '() #f ]))

  (define (check-rb upper lower)
    (=>
     (and (gate? (car (rule-rb upper lower)))
          (in (toehold 0) (duplex-strand-domain-list (gate-duplex (car (rule-rb upper lower))))))
     (and (in (toehold 0) (upper-strand-domain-list upper))
          (in (complement (toehold 0)) (lower-strand-domain-list lower)))))

  (solver-check check-rb (list upper lower))

  (define test-gate
    (gate
     (upper-strand (domain-cat-?? 2))
     (lower-strand (domain-cat-?? 2))
     (duplex-strand (domain-cat-?? 2))
     (lower-strand (domain-cat-?? 2))
     (upper-strand (domain-cat-?? 2))))

  (define-symbolic a integer?)

  (define (check-rc g a)
    (=>
     ; if...
     (and
      ; (toehold a) is at the front of the upper right...
      (equal? (toehold a) (car (upper-strand-domain-list (gate-right-upper g))))
      ; and (complement (toehold a) is at the front of the lower right...
      (equal? (complement (toehold a)) (car (lower-strand-domain-list (gate-right-lower g)))))

     ; then (toehold a) should be and the end of the resulting gate duplex
     (equal? (toehold a) (last (duplex-strand-domain-list (gate-duplex (car (rule-rc g))))))))

  (solver-check check-rc (list test-gate a))

  (define (remove-left-lower g)
    (match g [(gate lu ll s rl ru)
              (gate lu `() s rl ru)]
      [_ g]))

  (define (check-rga1 g s1 s2)
    ( =>
      ; if...
      (and
         ; s1 and s2 are reducible via rb
         (solver-check check-rb (list (upper-strand s1) (lower-strand s2)))
         ; s1 is the left lower strand of g
         (equal? s1 (gate-left-lower g)))
      ; then rga1 should, when fed g and an upper strand containing s2...
      (and
       ; produce a gate...
       (gate? (rule-rga1 g (upper-strand s2)))
       ; that has the result of reducing s1 and s2 as its first part...
       (equal? (rule-rb (upper-strand s1) (upper-strand s2)) (car (rule-rga1 g (upper-strand s2))))
       ; and s1 is no longer in the second part
       (equal? (cdr (rule-rga1 g (upper-strand s2))) (remove-left-lower g))
      )
    )
    )

  (define s2 (domain-cat-?? 2))
  (define s1 (domain-cat-?? 2))

  (solver-check check-rga1 (list test-gate s1 s2))

  )
