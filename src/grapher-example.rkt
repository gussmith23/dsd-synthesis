#lang rosette/safe

#|-----this module currently not in use-----|#

; This file serves as an example of how to use grapher.rkt

(require "grapher.rkt" "dna-syntax.rkt" "interpreter.rkt")

#;(define and-test-system
    (list
     (upper-strand (list (toehold 1) 2))
     (upper-strand (list 3 (toehold 4)))
     (gate (upper-strand '())
           (lower-strand (list (complement (toehold 1))))
           (duplex-strand (list 2 3))
           (lower-strand (list (complement (toehold 4))))
           (upper-strand '()))))

; Uncomment the two lines below to print drawing of graph
;(dot "/usr/local/bin/dot") ; your dot installation directory
;(dot-string->png (get-dot-graph-string (compile and-test-system (initial-state -1))))

#;(printf (get-dot-graph-string (compile and-test-system (initial-state -1))))