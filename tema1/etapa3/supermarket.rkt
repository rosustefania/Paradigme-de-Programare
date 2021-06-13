#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (counter index 0 0 empty-queue))

(define (update f counters index)
  (map (lambda(counter) (if (equal? (counter-index counter) index)
                            (f counter)
                            counter))
         counters))

(define tt+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index tt et queue)
    (struct-copy counter C [tt (+ tt minutes)])]))))

(define et+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index tt et queue)
    (struct-copy counter C [et (+ et minutes)])]))))


(define (add-to-counter name items)     
  (lambda (C)                             
    (counter (counter-index C)
             (+ (counter-tt C) items)
             (if (queue-empty? (counter-queue C))
                 (+ (counter-et C) items)
                 (counter-et C))
             (enqueue (cons name items) (counter-queue C)))))


(define (general-min-helper counters index acc time)
  (cond
    ((null? counters) (cons index acc))
    ((< (time (car counters)) acc) (general-min-helper (cdr counters) (counter-index (car counters))
                                                               (time(car counters)) time))
    (else (general-min-helper (cdr counters) index acc time))))

(define (general-min counters time)
  (if (null? counters)
      null
      (general-min-helper (cdr counters) (counter-index (car counters)) (time (car counters)) time)))
      

(define (min-tt counters) (general-min counters counter-tt)) 
(define (min-et counters) (general-min counters counter-et))

(define (remove-first-from-counter C)   
    (counter (counter-index C)
             (if (queue-empty? (counter-queue C))
                 0
                 (- (counter-tt C) (counter-et C)))
             (if (queue-empty? (dequeue (counter-queue C)))
                 0
                (cdr (top (dequeue (counter-queue C)))) )                                    
             (dequeue (counter-queue C))))  


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (lambda (C)
    (counter (counter-index C)
             (if(> (counter-tt C) minutes)
                (- (counter-tt C) minutes)
                0)             
             (if(> (counter-et C) minutes)
                (- (counter-et C) minutes)
                0)             
             (counter-queue C))))

(define (remove-pass-time minutes C)
    (cond
      ((< minutes (counter-et C)) ((pass-time-through-counter minutes) C))
      ((>= minutes (counter-tt C)) (empty-counter (counter-index C)))
      (else (remove-pass-time (- minutes (counter-et C)) (remove-first-from-counter C)))
      ))

(define (remove-from-all-counters-helper minutes counters acc)
  (if (null? counters)
      acc
      (remove-from-all-counters-helper minutes (cdr counters) (append acc (list (remove-pass-time minutes (car counters)))))))

(define (remove-from-all-counters minutes counters)
  (remove-from-all-counters-helper minutes counters null))


(define (get-exits-helper minutes exits C)
  (cond
    ((< minutes (counter-et C)) exits)
    (else (if (queue-empty? (counter-queue C))
                                    exits
                                    (get-exits-helper (- minutes (counter-et C)) (append exits (list (list (counter-index C) (car(top (counter-queue C)))(- minutes (counter-et C))))) (remove-first-from-counter C) )))))


(define (get-exits minutes exits counters)
  (if (null? counters)
      exits
      (get-exits minutes (append exits (get-exits-helper minutes null (first counters))) (cdr counters))))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define ettt+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index tt et queue)
    (struct-copy counter C [tt (+ tt minutes)][et (+ et minutes)])]))))

(define (sum-tt counters)
  (if (null? counters)
      0
      (+ (sum-tt (cdr counters)) (counter-tt (car counters)))))

(define (average-tt fast-counters slow-counters)
  (cond
    ((null? fast-counters) (if (null? slow-counters)
                               0
                               (/ (sum-tt slow-counters) (length slow-counters))))
    (else (if (null? slow-counters)
              (/ (sum-tt fast-counters) (length fast-counters))
              (/ (+ (sum-tt fast-counters) (sum-tt slow-counters)) (+ (length fast-counters) (length slow-counters)))))))
(define (get-exits-list-helper exits acc)
  (if (null? exits)
      acc
      (get-exits-list-helper (cdr exits) (append acc (list (cons (first (car exits)) (second (car exits))))))))

(define (get-exits-list exits)
  (get-exits-list-helper (sort-crono exits) null))

                 
(define (sort-crono list)
  (sort list
        (lambda (l1 l2) (if (= (third l1) (third l2))
                            (< (car l1) (car l2))
                            (> (third l1) (third l2))))))

(define (serve-helper requests fast-counters slow-counters exits)

  (if (null? requests)
      (cons exits (append fast-counters slow-counters))
      (match (car requests)
        [(list 'ensure average) (if (> (average-tt fast-counters slow-counters) average)
                                     (serve-helper requests fast-counters (append slow-counters (list (counter (add1 (+ (length fast-counters) (length slow-counters))) 0 0 empty-queue))) exits)
                                     (serve-helper (cdr requests) fast-counters slow-counters exits))                                                                                                 
                                     ]        
        [(list name n-items) (cond
                              ((> n-items ITEMS) (serve-helper (cdr requests) fast-counters (update (lambda (C) ((add-to-counter name n-items) C)) slow-counters (car (min-tt slow-counters))) exits))
                              (else (cond
                                      ((<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters))) (serve-helper (cdr requests) (update (lambda (C) ((add-to-counter name n-items) C)) fast-counters (car (min-tt fast-counters))) slow-counters exits))
                                      (else (serve-helper (cdr requests) fast-counters (update (lambda (C) ((add-to-counter name n-items) C)) slow-counters (car (min-tt slow-counters))) exits)))))]
        [(list 'delay index minutes) (cond
                                       ((null? (filter (lambda (C) (= (counter-index C) index)) slow-counters)) (serve-helper (cdr requests) (update (lambda (C) ((ettt+ C) minutes)) fast-counters index) slow-counters exits))
                                       (else (serve-helper (cdr requests) fast-counters (update (lambda (C) ((ettt+ C) minutes)) slow-counters index) exits)))]
        [ x (serve-helper (cdr requests) (remove-from-all-counters x fast-counters) (remove-from-all-counters x slow-counters) (append exits (get-exits-list (get-exits x null (append slow-counters fast-counters)))))]
            
            )))

 (define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters null))

    
