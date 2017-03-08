#lang rosette/safe

(require "dna-syntax.rkt")
(require "interpreter.rkt")
(require "dna-string-parser.rkt")
(require "reduction-rules.rkt")

; Example 1 -- AND gate system
(define (and-system? system)
  (let* ([reactions (enumerate-reactions system)]
         [inputs (filter upper-strand? system)]
         [input-1 (first inputs)]
         [input-2 (second inputs)]
         [outputs (map reaction-strand-out reactions)])

    (and
     ; the inputs are different
     (not (equal? input-1 input-2))

     ; there exists an output where
     (ormap
      (lambda (output)

        (let ([parent-reactions (parents output reactions)])
          
          (and
           ; the output is not null
           (not (null? output))
           
           ; at least one parent reaction is non null
           (ormap (lambda (parent) (not (null? parent))) parent-reactions)

           ; and each parent is either null or requires both inputs
           (andmap
            (lambda (parent)
              (or (null? parent)
                  (and
                   (reaction-requires parent input-1)
                   (reaction-requires parent input-2))))
            parent-reactions))))
            
        outputs))))

(define and-system
  (dsd->system
   #<<EOF
(<1^ 2>
|<3 4^>
|{1^*}[2 3]{4^*}
)
EOF
   ))

(define sketch-1
  (dsd->system
   #<<EOF
(<?? ??>
|<?? ??>
|{??}[?? ??]{??}
)
EOF
   ))

(define (solve-system property sketch)
  (define f (property sketch))
  (define sol (solve (assert f)))
  (if (sat? sol)
      (display (system->dsd (evaluate sketch sol)))
      (display (unsat)))
  (clear-asserts!))

; Example 2 -- OR gate system
(define (or-system? system)
  (let* ([reactions (enumerate-reactions system)]
         [inputs (filter upper-strand? system)]
         [input-1 (first inputs)]
         [input-2 (second inputs)]
         [outputs (map reaction-strand-out reactions)])

    (and
     ; the inputs are different
     (not (equal? input-1 input-2))

     ; there exists an ouptut where
     (ormap
      (lambda (output)

        (let ([parent-reactions (parents output reactions)])
          
          (and
           ; the output is not null
           (not (null? output))

           ; there exists a parent reaction that needs input 1 but not input 2
           (ormap
            (lambda (parent)
              (and
               (reaction-requires parent input-1)
               (not (reaction-requires parent input-2))))
            parent-reactions)

           ; there exists a parent reaction that needs input 2 but not input 1
           (ormap
            (lambda (parent)
              (and
               (reaction-requires parent input-2)
               (not (reaction-requires parent input-1))))
            parent-reactions))))

            
        outputs))))




(define or-system
  (dsd->system
   #<<EOF
(<0 t^ 1 t^>
|<0 t^ 2 t^>
|{t^*}[1 t^]<3 t^>
|{t^*}[2 t^]<3 t^>
|{t^*}[3 t^]<4 t^>
)
EOF
   ))

(define sketch-2
  (dsd->system
   #<<EOF
(<?? ?? ?? ??>
|<?? ?? ?? ??>
|{??}[?? ??]<?? ??>
|{??}[?? ??]<?? ??>
|{??}[?? ??]<?? ??>
EOF
   ))



