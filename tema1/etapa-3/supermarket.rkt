; Ispas Alexandra-Petrina 322CDb
; alexandra.ispas@stud.acs.upb.ro

#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue) #:transparent)

; structura formata din 2 liste care retine:
; o lista de counters care contin index, tt, et si persoana
; eliminata de la casa, precum si o lista a caselor in forma
; in care acestea raman in urma operatiilor efectuate asupra lor
(define-struct eliminated (el re) #:transparent)

(define (empty-counter index)          
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (map (λ(x)
         (if (equal? (counter-index x) index)
             (f x) x)) counters))

; aplica functia f asupra tuturor caselor din lista
(define (update-all-counters f counters)
  (apply f counters))

(define (tt+ minutes)
  (λ (C)
    (struct-copy counter C
                 [tt (+ minutes (counter-tt C))]))) 

(define (et+ minutes)
  (λ(C)
    (struct-copy counter C
                 [et (+ minutes (counter-et C))])))

(define (add-to-counter name n-items)     
  (λ (C)
    (let* ([newC (struct-copy counter C [queue (enqueue (cons name n-items)
                                  (counter-queue C))])])
      (if(queue-empty? (dequeue (counter-queue newC))) ((tt+ n-items) ((et+ n-items) newC)) 
       ((tt+ n-items) newC)))))   

(define (get-min field)
  (λ(counters)
    (define C (foldl (λ(x acc) 
                       (if (< (field x) (field acc)) x acc)) 
                     (car counters) (cdr counters))) 
    (cons (counter-index C) (field C))))

(define (min-tt counters) ((get-min counter-tt) counters))
(define (min-et counters) ((get-min counter-et) counters)) 

; functia care scoate prima persoana de la casa
(define (remove-first-from-counter C)   
  (cond [(queue-empty? (counter-queue C)) C]
        [(queue-empty? (dequeue (counter-queue C))) (empty-counter (counter-index C))] 
        [else (struct-copy counter C [tt (- (counter-tt C) (counter-et C))]
                           [et (if (queue-empty? (dequeue (counter-queue C))) 0 (cdr(top(dequeue (counter-queue C)))))]
                           [queue (dequeue (counter-queue C))])]))  

; funcție care calculează starea unei case după un număr dat de minute.
; deci va avea efect doar asupra câmpurilor tt și et.
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond [(and (>= (counter-tt C) minutes) (>= (counter-et C) minutes))
           ((et+ (* -1 minutes))((tt+ (* -1 minutes)) C))]
          [(>= (counter-tt C) minutes) ((et+ (* -1 (counter-et C)))((tt+ (* -1 minutes)) C))]
          [(>= (counter-et C) minutes) ((et+ (* -1 minutes ))((tt+ (* -1 (counter-tt C))) C))]
          [else ((et+ (* -1 (counter-et C))) ((tt+ (* -1 (counter-tt C))) C))])))

;functie care intarzie un counter
;modificand et si tt
(define (delay-counter minutes)
  (λ(C)
    ((et+ minutes) ((tt+ minutes) C))))

; cauta casa la care s-ar aseza un nou client
(define(get-fastest-counter fast-counters slow-counters items)
  (if(<= items ITEMS)
     (car (min-tt (append fast-counters slow-counters)))
     (car (min-tt slow-counters))))

; sorteza lista de clienti iesiti de la case
; in fucntie de et-ul casei respective
(define (sort-et counters)
  (if (null? counters)
      counters
      (insert (car counters) (sort-et (cdr counters)))))

(define (insert x L)
  (cond[(null? L) (cons x L)]
       [(and(equal?(counter-index x) (counter-index (car L)))
            (> (counter-tt x) (counter-tt (car L))))
        (cons x L)]
       [(and(not(equal?(counter-index x) (counter-index (car L))))
            (< (counter-et x) (counter-et (car L)))) (cons x L)]
       [(and( < (counter-index x) (counter-index (car L)))
            (equal? (counter-et x)(counter-et (car L)))) (cons x L)]
       
       [else      (cons (car L) (insert x (cdr L)))]))


(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters
                 slow-counters '()))

; functia ajutatoare pentru functia 'serve'
(define (serve-helper requests fast-counters slow-counters acc)

  ;verifica daca index-ul apartine unei case rapide
  (define (is-fast? index)
    (not(null? (filter (λ(x)
                         (equal? (counter-index x) index)) fast-counters))))
  
  (define (delay index minutes)
    (if(is-fast? index)
       (serve-helper (cdr requests) (update (delay-counter minutes) fast-counters index) slow-counters acc)
       (serve-helper (cdr requests) fast-counters (update (delay-counter minutes) slow-counters index) acc)))
  
  (define (add name n-items)
    (define index (get-fastest-counter fast-counters slow-counters n-items))
    (if(is-fast? index)
       (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters index) slow-counters acc)
       (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters index) acc)))
 
  (define (ensure average)
    (let* ( [sum-tt (foldl (λ(x acc)  ; suat tt-urilor caselor existente
                             (+ (counter-tt x) acc)) 0 (append fast-counters slow-counters))]
            [nr-of-counters (ceiling (/ (- sum-tt   ;numarul caselor ce trebuie adaugate
                                           (* (length (append fast-counters slow-counters)) average)) average))]
            [indexes (range (add1 (counter-index (last slow-counters)))  ;indecsii ce trebuie folositi
                            (+ 1 (counter-index (last slow-counters)) nr-of-counters))])
      (serve-helper (cdr requests) fast-counters
                    (foldl (λ(index acc) (append acc (list (make-counter index 0 0 empty-queue)))) slow-counters indexes) acc)))


  ; simuleaza trecerea timpului la case
  ; pastreaza persoanele eliminate in lista 'elim', iar starea finala a caselor
  ; este salvata in lista 'rem'
  ; la final intoarce o structura de tip 'eliminated' formata din cele e liste (elim, rem)
  (define (pass-time-structure minutes minutes-copy counters elim rem)  
    (if(null? counters)
       (make-eliminated elim rem)
       (cond [(and (<= (counter-et (car counters) ) minutes)
                   (not(queue-empty?(counter-queue(car counters)))))      
              (pass-time-structure (- minutes (counter-et(car counters))) 
                                   minutes-copy                          
                                   (update remove-first-from-counter counters (counter-index (car counters)))
                                   (cons (struct-copy counter (car counters)
                                                      [queue (top (counter-queue (car counters)))]) elim) rem)]

             [else (pass-time-structure minutes-copy minutes-copy (cdr counters) elim
                                   (append rem (list ((pass-time-through-counter minutes) (car counters)))))]))) 

  ;intorce lista de perechi intre index-ul casei si numele persoanei eliminate 
  (define(get-acc counters acc) 
    (if(null? counters) 
       acc
       (get-acc (cdr counters)
                (append acc(list (cons (counter-index (car counters))
                                       (car(counter-queue (car counters))))))))) 

  (define(pass-time minutes)
    (let* ([ elim-fast (pass-time-structure minutes minutes fast-counters '() '())]
      [elim-slow (pass-time-structure minutes minutes slow-counters '() '())]) 

    (serve-helper (cdr requests) (eliminated-re elim-fast) (eliminated-re elim-slow) 
                  (append acc (get-acc (sort-et (append (eliminated-el elim-fast)
                                                        (eliminated-el elim-slow))) '())))))

  (if(null? requests)
     (cons acc (append fast-counters slow-counters))
     (match (car requests)
       [(list 'ensure average) (ensure average)]
       [(list name n-items) (add name n-items)]
       [(list 'delay index minutes) (delay index minutes)]
       [minutes (pass-time minutes)])))
