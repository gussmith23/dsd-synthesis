#lang rosette

; This file provides predicates that are able to check whether
; a given input corresponds to a non-terminal in our defined
; DNA strand displacement grammar

(provide id?
         sequence?
         domain?
         upper?
         lower?
         strand?
         gate?
         species?
         system?)

; Syntax of the DNA strand displacement language
; <id>       := NAME
;
; <sequence> := (S <id>)
;             | (T <id>)
;             | (C (S <id>))
;             | (C (T <id>))
;
; <domain>   := (<sequence>*)
;
; <upper>    := (U domain)
; <lower>    := (L domain)
;
; <strand>   := upper
;             | lower
;
; <gate>     := (gate <upper> <lower> <domain> <lower> <upper>)
;             | (: <gate> <gate>)
;             | (:: <gate> <gate>)
;
; <species>  := <strand>
;             | <gate>
;
; <system>   := (<species>+)

(define id? symbol?)

(define (sequence? input)
  (match input
    [`(S ,id) (id? id)]
    [`(T ,id) (id? id)]
    [`(C (S ,id)) (id? id)]
    [`(C (T ,id)) (id? id)]
    [_ #f]))

(define (domain? input)
  (and (list? input) (andmap sequence? input)))

(define (upper? input)
  (match input
    [`(U ,domain) (domain? domain)]
    [_ #f]))
(define (lower? input)
  (match input
    [`(L ,domain) (domain? domain)]
    [_ #f]))

(define (strand? input)
  (or (upper? input) (lower? input)))

(define (gate? input)
  (match input
    [`(gate ,ul ,ll ,domain ,lr ,ur)
     (upper? ul)
     (lower? ll)
     (domain? domain)
     (lower? lr)
     (upper? ur)]
    [`(: ,left-gate ,right-gate)
     (and (gate? left-gate) (gate? right-gate))]
    [`(:: ,left-gate ,right-gate)
     (and (gate? left-gate) (gate? right-gate))]
    [_ #f]))

(define (species? input)
  (or (strand? input) (gate? input)))

(define (system? input)
  (and (list? input) (andmap species? input)))
