; Ispas Alexandra-Petrina 322CDb

#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

; am adaugat campul 'bl' care este de tip boolean
; false inseamna ca nu este blocata casa
(define-struct counter (index bl tt et queue) #:transparent)
(define-struct eliminated (el re) #:transparent)

(define (empty-counter index)
  (make-counter index #f 0 0 empty-queue))

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
    (let ([newC (struct-copy counter C [queue (enqueue (cons name n-items) (counter-queue C))])])
      (if(queue-empty? (dequeue (counter-queue newC))) ((tt+ n-items) ((et+ n-items) newC)) 
         ((tt+ n-items) newC)))))   

(define (get-min field)
  (λ(counters)
    (let* ([C (foldl (λ(x acc) (if (< (field x) (field acc)) x acc)) 
               (car counters) (cdr counters))]) 
      (cons (counter-index C) (field C)))))

(define (min-tt counters) ((get-min counter-tt) counters))
(define (min-et counters) ((get-min counter-et) counters)) 

; functia care scoate prima persoana de la casa
(define (remove-first-from-counter C)   
  (cond [(queue-empty? (counter-queue C)) C]   ;am modificat astfel incat sa pastreze valoarea precedenta a campului bl
        [(queue-empty? (dequeue (counter-queue C))) (make-counter (counter-index C) (counter-bl C) 0 0 empty-queue)] 
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

; functie care intarzie un counter
; modificand et si tt
(define (delay-counter minutes)
  (λ(C)
    ((et+ minutes) ((tt+ minutes) C))))

; cauta casa la care s-ar aseza un nou client
; inainte de a cauta indexul, filtrez lista de counters astfel incat sa nu fie case blocate
(define(get-fastest-counter fast-counters slow-counters items)
  (let* ([all-counters (filter (λ(C) (not(counter-bl C))) (append fast-counters slow-counters))]
         [slow (filter (λ(C) (not(counter-bl C))) slow-counters)])
    (if(<= items ITEMS)
       (car (min-tt all-counters))
       (car (min-tt slow)))))

                                    
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters
                slow-counters '()))

(define (serve-helper requests fast-counters slow-counters acc)

  ; verifica daca index-ul apartine unei case rapide
  (define (is-fast? index)
    (not(null? (filter (λ(x) (equal? (counter-index x) index)) fast-counters))))
  
  (define (delay index minutes)
    (if(is-fast? index)
       (serve-helper (cdr requests) (update (delay-counter minutes) fast-counters index) slow-counters acc)
       (serve-helper (cdr requests) fast-counters (update (delay-counter minutes) slow-counters index) acc)))
  
  (define (add name n-items)
    (let* ([index (get-fastest-counter fast-counters slow-counters n-items)])
      (if(is-fast? index)
         (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters index) slow-counters acc)
         (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters index) acc))))
 
  (define (ensure average) ; filtrez casele blocate inainte de a calcula ttmed
    (let* ([counters (filter (λ(C) (not(counter-bl C))) (append fast-counters slow-counters))]
           [sum-tt (foldl (λ(x acc) (+ (counter-tt x) acc)) 0 counters)]
           [nr-of-counters (ceiling (/ (- sum-tt (* (length counters) average)) average))]
           [indexes (range (add1 (counter-index (last slow-counters))) 
                           (+ 1 (counter-index (last slow-counters)) nr-of-counters))])
      (serve-helper (cdr requests) fast-counters
                    (foldl (λ(index acc) (append acc (list (empty-counter index)))) slow-counters indexes) acc))) 


  ; simuleaza trecerea timpului la case
  ; pastreaza persoanele eliminate in lista 'elim', iar starea finala a caselor
  ; este salvata in lista 'rem'
  ; la final intoarce o structura de tip 'eliminated' formata din cele 2 liste (elim, rem)
  (define (pass-time-structure minutes minutes-copy counters elim rem)  
    (if(null? counters)
       (make-eliminated elim rem)
       (cond [(and (<= (counter-et (car counters) ) minutes)
                   (not(queue-empty?(counter-queue(car counters)))))      
              (pass-time-structure (- minutes (counter-et(car counters))) 
                                   minutes-copy                          
                                   (update remove-first-from-counter counters (counter-index (car counters)))
                                   (append elim (list (struct-copy counter (car counters)
                                                                   [queue (top (counter-queue (car counters)))]))) rem)]

             [else (pass-time-structure minutes-copy minutes-copy (cdr counters) elim
                                        (append rem (list ((pass-time-through-counter minutes) (car counters)))))])))

  (define(get-acc counters acc) 
    (if(null? counters)  acc
       (get-acc (cdr counters)
                (append acc(list (cons (counter-index (car counters))
                                       (car(counter-queue (car counters))))))))) 

; pentru a obtine clientii iesiti de la case in ordine
; simulez trecerea timpului cu cate 1 minut, urmand sa fie incrementat pana la 'minutes'
  (define(pass-time minutes)
    (let ([elim (let loop( [mins 1]            ;incep de la 1
                           [copy-mins minutes] ;timpul care mai trebuie sa treaca pentru a simula tot intervalul
                           [counters (make-eliminated '() (append fast-counters slow-counters))])

                  (if(zero? copy-mins) ; am simulat tot intervalul primit
                     counters
                     (if(equal? counters (pass-time-structure mins mins  ;verific daca a iesit cineva de la casa la ultimul apel
                                                                  (eliminated-re counters)
                                                                  (eliminated-el counters) '()))
                        ; nu a iesit nimeni -> maresc intervalul
                        (loop (add1 mins) copy-mins (pass-time-structure mins mins (eliminated-re counters) (eliminated-el counters) '()))
                        ; a iesit cineva -> pastrez acelasi interval, dar scad timpul ramas din simulare
                        (loop 1 (- copy-mins mins) (pass-time-structure mins mins (eliminated-re counters) (eliminated-el counters) '())))))]
          ; resimulez pentru casele fast si slow separat
          [fast (pass-time-structure minutes minutes fast-counters '() '())]
          [slow (pass-time-structure minutes minutes slow-counters '() '())])
      
      (serve-helper (cdr requests) (eliminated-re fast) (eliminated-re slow) 
                    (append acc (get-acc (eliminated-el elim) '())))))

  ; inchide o casa
  (define close-counter
    (λ(C) (struct-copy counter C [bl #t])))
    
  (define (close index)
    (if(is-fast? index)
       (serve-helper (cdr requests) (update close-counter fast-counters index) slow-counters acc)
       (serve-helper (cdr requests) fast-counters (update close-counter slow-counters index) acc)))
  

  (if(null? requests)
     (cons acc (foldl (λ(C acc) (append acc (list(cons(counter-index C) (counter-queue C))))) '()
                      (filter (λ(C) (not(queue-empty? (counter-queue C)))) (append fast-counters slow-counters))))
     (match (car requests)
       [(list 'ensure average) (ensure average)]
       [(list 'close index) (close index)]
       [(list name n-items) (add name n-items)]
       [(list 'delay index minutes) (delay index minutes)]
       [minutes (pass-time minutes)])))
