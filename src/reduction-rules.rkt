#lang rosette/safe

(require rosette/lib/match)
(require "dna-syntax.rkt")
(require "strand-manipulation.rkt")

; provides only the "infinite" semantics
(provide

 ; binary rules:
 rule-rp
 rule-rgl

 (struct-out reaction)
 allow-unbindings!
 disallow-unbindings!

 toe-search ; for detailed semantics
 )

(define allow-unbindings #t)
(define (allow-unbindings!) (set! allow-unbindings #t))
(define (disallow-unbindings!) (set! allow-unbindings #f))

(struct reaction (strand-in gate-in strand-out gate-out) #:transparent)
(define (no-reaction s1 s2) (reaction s1 s2 '() '()))

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

; return the longest common prefix of two sequences,
; as well as the two suffixes
(define (prefix-match branch gate)
  (define (prefix-match-aux branch gate prefix)

    (if (and (not (null? branch))
             (not (null? gate))
             (equal? (car branch) (car gate)))

        ; heads match, add prefix and continue
        (prefix-match-aux (cdr branch) (cdr gate) (cons (car branch) prefix))

        ; heads don't match, stop
        (list (reverse prefix) branch gate)))

  (prefix-match-aux branch gate '()))

(define (rule-rp strand-in gate-in)
  (match* (strand-in gate-in)
    [ ((upper-strand upper-domains)

       (gate
        (upper-strand lu)
        (lower-strand ll-domains)
        (duplex-strand s)
        (lower-strand rl)
        (upper-strand ru)))

      (match (toe-search upper-domains ll-domains)
        ; no matching toeholds -- do nothing
        [ '() (no-reaction strand-in gate-in) ]

        ; matching toeholds -- check conditions
        [ (list
           (list upper-left toe upper-right)
           (list lower-left ctoe lower-right))


          (if (and
               ;toehold must not be last domain in upper strand
               (not (null? upper-right))
               ; but it must be last domain in lower strand
               (null? lower-right)
               ; reaction must lead to migration or displacement
               (equal? (car upper-right) (car s)))

              (match (prefix-match upper-right s)

                [ (list prefix rest-branch rest-gate)

                  (cond
                    ; nothing else on gate; strand displacement
                    [ (null? rest-gate)

                      (reaction
                       strand-in gate-in
                       (upper-strand (append lu prefix ru))
                       (gate
                        (upper-strand upper-left)
                        (lower-strand lower-left)
                        (duplex-strand  (cons toe prefix))
                        (lower-strand rl)
                        (upper-strand rest-branch))) ]

                    ; only thing left is toehold -- trigger unbinding
                    ; NB: since unbinding is not a "fast reaction"
                    ; it should not technically be merged in here
                    ; according to the "default" semantics,
                    ; but it is okay for this prototype.
                    [ (and allow-unbindings
                           (or (toehold? (car rest-gate))
                               (and (complement? (car rest-gate))
                                    (toehold?
                                     (complement-id-or-toehold (car rest-gate)))))
                           (null? (cdr rest-gate)))

                      (reaction
                       strand-in gate-in
                       (upper-strand (append lu prefix rest-gate ru))
                       (gate
                        (upper-strand upper-left)
                        (lower-strand lower-left)
                        (duplex-strand (cons toe prefix))
                        (lower-strand (cons (complement-of-domain (car rest-gate)) rl))
                        (upper-strand rest-branch))) ]

                    ; stuff left on gate; branch migration
                    [ else
                      (reaction
                       strand-in gate-in
                       '()
                       (gate:
                        (gate
                         (upper-strand upper-left)
                         (lower-strand lower-left)
                         (duplex-strand (cons toe prefix))
                         (lower-strand '())
                         (upper-strand rest-branch))
                        (gate
                         (upper-strand (append lu prefix))
                         (lower-strand '())
                         (duplex-strand rest-gate)
                         (lower-strand rl)
                         (upper-strand ru)))) ]) ])

              ; conditions do not hold -- rule does not fire
              (no-reaction strand-in gate-in)) ]) ]

    ; rule did not match
    [ (_ _) (no-reaction strand-in gate-in) ]))

(define (rule-rgl strand-in gate-in)
  (match* (strand-in gate-in)
    [ (strand-in (gate: left right))
      (match (rule-rp strand-in left)
        [ (reaction _ _ strand-out gate-out)

          (cond
            ; result was strand displacement
            [ (not (null? strand-out)) (reaction strand-in gate-in strand-out (gate: gate-out right)) ]

            ; result was branch migration
            [ (not (null? gate-out)) (reaction strand-in gate-in '() (gate: gate-out right)) ]

            ; no reaction
            [ else (no-reaction strand-in gate-in) ]) ]) ]

    [ (_ _) (no-reaction strand-in gate-in) ]))

