#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)
(define MIN_TT_COUNTER (cons 0 100))


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.

; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define-struct counter (index status tt et queue) #:transparent)

(define (empty-counter index)
  (counter index 1 0 0 empty-queue))

(define (update f counters index)
  (map (lambda(counter) (if (equal? (counter-index counter) index)
                            (f counter)
                            counter))
         counters))

(define tt+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index status tt et queue)
    (struct-copy counter C [tt (+ tt minutes)])]))))

(define et+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index status tt et queue)
    (struct-copy counter C [et (+ et minutes)])]))))


(define (add-to-counter name items)     
  (lambda (C)                             
    (counter (counter-index C)
             (counter-status C)
             (+ (counter-tt C) items)
             (if (queue-empty? (counter-queue C))
                 (+ (counter-et C) items)
                 (counter-et C))
             (enqueue (cons name items) (counter-queue C))
             )))


(define (general-min-helper counters index acc time)
  (cond
    ((null? counters) (cons index acc))
    ((= (counter-status (car counters)) 1) 
     (if (< (time (car counters)) acc)
         (general-min-helper (cdr counters) (counter-index (car counters)) (time(car counters)) time)
         (general-min-helper (cdr counters) index acc time)))
    (else (general-min-helper (cdr counters) index acc time))))

(define (get-open-counters counters acc)
  (if (null? counters)
      acc
      (if (= (counter-status (car counters)) 1)
          (get-open-counters (cdr counters) (append acc (list (car counters))))
          (get-open-counters (cdr counters) acc))))

(define (general-min counters time)
  (if (null? (get-open-counters counters null))
      MIN_TT_COUNTER
      (general-min-helper counters (counter-index (car (get-open-counters counters null))) (time (car (get-open-counters counters null))) time)))
      

(define (min-tt counters) (general-min counters counter-tt)) 
(define (min-et counters) (general-min counters counter-et))

(define (remove-first-from-counter C)   
    (counter (counter-index C)
             (counter-status C)
             (if (queue-empty? (counter-queue C))
                 0
                 (- (counter-tt C) (counter-et C)))
             (if (queue-empty? (dequeue (counter-queue C)))
                 0
                (cdr (top (dequeue (counter-queue C)))) )                                    
             (dequeue (counter-queue C))
             ))

(define (pass-time-through-counter minutes)
  (lambda (C)
    (counter (counter-index C)
             (counter-status C)
             (if(> (counter-tt C) minutes)
                (- (counter-tt C) minutes)
                0)             
             (if(> (counter-et C) minutes)
                (- (counter-et C) minutes)
                0)             
             (counter-queue C)
             )))

(define (remove-pass-time minutes C)
    (cond
      ((< minutes (counter-et C)) ((pass-time-through-counter minutes) C))
      ((>= minutes (counter-tt C)) (struct-copy counter C [tt 0] [et 0] [queue empty-queue]))
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


(define (get-exits minutes exits counters )
  (if (null? counters)
      exits
      (get-exits minutes (append exits (get-exits-helper minutes null (first counters))) (cdr counters))))


(define ettt+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index status tt et queue)
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

(define close-counter
  (lambda (C)
    (struct-copy counter C [status 0])))


(define (serve-helper requests fast-counters slow-counters exits)
  (if (null? requests)
      (cons exits (append fast-counters slow-counters))
      (match (car requests)       
        [(list 'ensure average) (if (> (average-tt (get-open-counters fast-counters null) (get-open-counters slow-counters null)) average)
                                     (serve-helper requests fast-counters (append slow-counters (list (counter (add1 (+ (length fast-counters) (length slow-counters))) 1 0 0 empty-queue))) exits)
                                     (serve-helper (cdr requests) fast-counters slow-counters exits))                                                                                                 
                                     ]
        [(list 'close index) (serve-helper (cdr requests) (update (lambda (C) (close-counter C)) fast-counters index) (update (lambda (C) (close-counter C)) slow-counters index) exits)]
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

(define (get-list counters acc)
  (if (null? counters)
      acc
      (if (queue-empty? (counter-queue (car counters)))
          (get-list (cdr counters) acc)
          (get-list (cdr counters) (append acc (list (cons (counter-index (car counters)) (counter-queue (car counters)))))))))

(define (serve requests fast-counters slow-counters)
      (cons (car (serve-helper requests fast-counters slow-counters null)) (get-list (cdr (serve-helper requests fast-counters slow-counters null)) null)))

