; Ispas Alexandra-Petrina 322CDb
; alexandra.ispas@stud.acs.upb.ro

#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; funcție care aplică o transformare casei cu un anumit index.
(define (update f counters index)
  (map (λ(x)
         (if (equal? (counter-index x) index)
             (f x) x)) counters))

; funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ minutes)
  (λ (C)
    (struct-copy counter C
                 [tt (+ minutes (counter-tt C))]))) 


;funcție care crește et-ul unei case cu un număr dat de minute.
(define (et+ minutes)
  (λ(C)
    (struct-copy counter C
                 [et (+ minutes (counter-et C))])))


;add-to-counter, modificand tt si et
(define (add-to-counter name n-items)
  (λ (C)
    ; adaug clientul in counter-queue
    (define newC (struct-copy counter C [queue (append (counter-queue C)
                                                       (list(cons name n-items) ))]))
    ;modific tt si et
    (if(null? (cdr (counter-queue newC))) ((tt+ n-items) ((et+ n-items) newC))
       ((tt+ n-items) newC)))) 


; funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
(define (get-min field)
  (λ(counters)
    (define C (foldl (λ(x acc) ; counter-ul care are cel mai mic et/tt
                       (if (< (field x) (field acc)) x acc)) (car counters) (cdr counters))) 
    (cons (counter-index C) (field C))))

(define (min-tt counters) ((get-min counter-tt) counters))
(define (min-et counters) ((get-min counter-et) counters )) 

; funcție care scoate prima persoană din coada unei case.
; tt-ul casei va scadea: tt -= et
; et-ul va deveni egal cu n-items al celei de-a doua persoane de la coada
; se elimina persoana din counter-queue
(define (remove-first-from-counter C)
  (if(null? (counter-queue C))
     C
     (struct-copy counter C [tt (- (counter-tt C) (counter-et C))]
                  [et (if (null? (cdr (counter-queue C))) 0 (cdadr (counter-queue C)))]
                  [queue (cdr (counter-queue C))])))

; delay aplicata pe un singur counter
; modifica tt si et al counter-ului dat
(define (delay-counter minutes)
  (λ(C)
    ((et+ minutes) ((tt+ minutes) C))))

; cauta casa la care s-ar aseza un nou client
(define(get-fastest-counter fast-counters slow-counters items)
  (if(<= items ITEMS)
     (car (min-tt (append fast-counters slow-counters)))
     (car (min-tt slow-counters))))

(define (ensure average)
  (λ (requests fast-counters slow-counters)
    (define sum-tt (foldl (λ(x acc)  ; sumt tt-urilor caselor existente
                            (+ (counter-tt x) acc)) 0 (append fast-counters slow-counters)))
   
    (define nr-of-counters (ceiling (/ (- sum-tt   ;numarul caselor ce trebuie adaugate
                                          (* (length (append fast-counters slow-counters)) average)) average)))
 
    (define indexes (range (add1 (counter-index (last slow-counters)))  ;indecsii ce trebuie folositi
                           (+ 1 (counter-index (last slow-counters)) nr-of-counters)))
    (serve (cdr requests) fast-counters
           (foldl (λ(index acc) (append acc (list (empty-counter index)))) slow-counters indexes))))


(define (serve requests fast-counters slow-counters)

  ;fuctie care verifica daca un index dat apartine unei case rapide
  (define (is-fast? index)
    (not(null? (filter (λ(x)
                         (equal? (counter-index x) index)) fast-counters))))

  (define (add name n-items)
    (define index (get-fastest-counter fast-counters slow-counters n-items))
    (if(is-fast? index)
       (serve (cdr requests) (update (add-to-counter name n-items) fast-counters index) slow-counters)
       (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters index))))
        
  
  (define (delay index minutes)
    (if(is-fast? index)
       (serve (cdr requests) (update (delay-counter minutes) fast-counters index) slow-counters)
       (serve (cdr requests) fast-counters (update (delay-counter minutes) slow-counters index))))


  (define (remove-first)
    (define counters  ; filtreaza casele care nu au niciun client
      (filter (λ(x) (not (null? (counter-queue x)))) 
              (append fast-counters slow-counters)))
  
    (define index     ; daca nu exista case cu clienti, trece mai departe
      (if (null? counters) (serve (cdr requests) fast-counters slow-counters)
          (car (min-et counters)))) 
    
    (if(is-fast? index)
       (serve (cdr requests) (update remove-first-from-counter fast-counters index) slow-counters)
       (serve (cdr requests) fast-counters (update remove-first-from-counter slow-counters index))))
  

  (if(null? requests)
     (append fast-counters slow-counters)
     (match (car requests)
       [(list 'ensure average) ((ensure average) requests fast-counters slow-counters)]
       [(list name n-items) (add name n-items)]
       [(list 'delay index minutes) (delay index minutes)]
       [(list 'remove-first) (remove-first)])))
