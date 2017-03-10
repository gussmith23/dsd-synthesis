#lang rosette

(require racket/draw)
(require "dna-syntax.rkt" "strand-manipulation.rkt" "dna-string-parser.rkt" "interpreter.rkt")

(provide get-dot-graph-string ; Returns dot graph string from given state struct s
         dot                  ; Sets the path given to it as the path to the dot executable
         dot-string->png      ; Takes a dot graph string and draws out the graph
         )

; Returns a string parsable by GraphViz dot to visualize the reaction in the given state s
#;(define (get-dot-graph-string s)
  ; Translation table
  (define species->name-table (make-hash))

  ; Factory for getting new labels
  (define current-letter 0)
  (define (get-new-letter)
    (define (loop current)
      (let ([letter (string (integer->char (+ (remainder current 26) (char->integer #\A))))])
        (if (>= current 26)
            (string-append (loop (- (quotient current 26) 1)) letter)
            letter)))
    (define letter (loop current-letter))
    (begin
      (set! current-letter (+ current-letter 1))
      letter
      ))

  ; Factory for getting a unique int
  (define current-temp 0)
  (define (get-new-temp)
    (define current current-temp)
    (begin
      (set! current-temp (+ current-temp 1))
      current))

  ; Get reactions and convert each species to a string
  (define reactions (state-reactions s))
  (define reaction-strings (map (lambda (r) (match r [(reaction i o) (reaction (map species->string i) (species->string o))])) reactions))

  ; Method to add species to the translation table
  (define (add-species species)
    (if (hash-has-key? species->name-table species)
        #t
        (hash-set! species->name-table species (get-new-letter))
    ))

  ; Add all species to the translation table
  (for ([r reaction-strings])
    (match r [(reaction i o) (begin (map add-species i) (add-species o))])
    )

  ; Create the dot digraph
  (define digraph (string-append "digraph {\n" (apply string-append (foldl (lambda (r acc) (append (match r
                                  [(reaction (list i1 i2) o) (let ([join_label (string-append "join_temp" (~a  (get-new-temp)))])
                                                               (list (string-append "  " join_label " [shape=box, label=\"\", fixedsize=true, width=0.1, height=0.1];\n")
                                                                     (string-append "  " (hash-ref species->name-table i1) "," (hash-ref species->name-table i2)
                                                                                    " -> " join_label " [arrowhead=none];\n")
                                                                     (string-append "  " join_label " -> " (hash-ref species->name-table o) ";\n")))]
                                  [(reaction (list i) o) (list (string-append "  " (hash-ref species->name-table i) " -> " (hash-ref species->name-table o) ";\n"))]
                                  ) acc)) null reaction-strings)) "}\n"))
  (define translations (string-append "/* Translations:\n"
                                      (apply string-append (map (lambda (p) (string-append (~a (cdr p) #:width 4 #:align 'right) " -> " (car p) "\n"))
                                                                (sort (hash->list species->name-table) string<? #:key cdr))) "*/"))
  (string-append digraph translations))


; This parameter holds the absolute path to the dot executable.
#;(define dot 
  (make-parameter (find-executable-path "dot")))

#;(define (dot-string->png graph-string)
  ; open dot
  (define-values (p p-out p-in p-err)
    (subprocess #f #f #f (dot) "-Tpng"))
  (fprintf p-in graph-string) ; write graph to dot
  (close-output-port p-in) ; close port
  (read-bitmap p-out 'png) ; draw
  )