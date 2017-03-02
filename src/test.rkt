#lang rosette

(require "dna-syntax.rkt")
(require "dna-string-parser.rkt")
(require rackunit)

(provide
 ; A function for running standard tests that directly compare
 ; results.
 ; Ensures that when tests fail, a comprehensible error message is
 ; printed.
 ; Actual and expected arguments should either be strings or valid dna
 ; structs.
 ; The optional msg argument is a string that's printed on failure as
 ; the name of the test.
 test-exact
)

(define (test-exact actual expected [msg ""])
  (if (valid-dna-struct? actual)
      (if (valid-dna-struct? expected)
          ; both are dna-structs
          (check-equal? (species->string actual) (species->string expected) msg)
          ; actual is a dna-struct, but expected is a string
          (check-equal? (species->string actual) expected msg))
      (if (valid-dna-struct? expected)
          ; only expected is a dna-struct
          (check-equal? actual (species->string expected) msg)
          ; neither are dna-structs
          (check-equal? actual expected msg))))
