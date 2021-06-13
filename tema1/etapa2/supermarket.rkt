#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (counter index 0 0 null))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (map (lambda(counter) (if (equal? (counter-index counter) index)

                            (f counter)
                            counter))
         counters))


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define tt+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index tt et queue)
    (struct-copy counter C [tt (+ tt minutes)])]))))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define et+
  (lambda (C)
    (lambda (minutes)
     (match C
    [(counter index tt et queue)
    (struct-copy counter C [et (+ et minutes)])]))))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define add-to-counter
  (lambda (C)
    (lambda (name)
      (lambda (n-items)
        (counter (counter-index C)
                 (+ (counter-tt C) n-items)
                 (if (null? (counter-queue C))
                     (+ (counter-et C) n-items)
                     (counter-et C))
                 (append (counter-queue C) (list(cons name n-items))))))))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)

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
      

(define (min-tt counters) (general-min counters counter-tt)) ; folosind funcția de mai sus
(define (min-et counters) (general-min counters counter-et)) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (counter (counter-index C)
           (if (> (length (counter-queue C)) 1)
               (- (counter-tt C) (counter-et C))
               0)
           (if (> (length (counter-queue C)) 1)
               (cdr (second (counter-queue C)))
               0)                                                                                  
           (remove (car (counter-queue C))(counter-queue C)))) 
    

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)

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
              
                                
                               
  

(define (serve requests fast-counters slow-counters)

  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) (if (> (average-tt fast-counters slow-counters) average)
                                     (serve requests fast-counters (append slow-counters (list (counter (add1 (+ (length fast-counters) (length slow-counters))) 0 0 null))))
                                     (serve (cdr requests) fast-counters slow-counters))                                                                                                 
                                     ]
        
        [(list name n-items) (cond
                              ((> n-items ITEMS) (serve (cdr requests) fast-counters (update (lambda (C) (((add-to-counter C) name) n-items)) slow-counters (car (min-tt slow-counters)))))
                              (else (cond
                                      ((<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters))) (serve (cdr requests) (update (lambda (C) (((add-to-counter C) name) n-items)) fast-counters (car (min-tt fast-counters))) slow-counters))
                                      (else (serve (cdr requests) fast-counters (update (lambda (C) (((add-to-counter C) name) n-items)) slow-counters (car (min-tt slow-counters))))))))]
        [(list 'delay index minutes) (cond
                                       ((null? (filter (lambda (C) (= (counter-index C) index)) slow-counters)) (serve (cdr requests) (update (lambda (C) ((ettt+ C) minutes)) fast-counters index) slow-counters))
                                       (else (serve (cdr requests) fast-counters (update (lambda (C) ((ettt+ C) minutes)) slow-counters index))))]
        [(list 'remove-first) (cond
                                  ((null? (filter (lambda (C) (not (null? (counter-queue C)))) fast-counters)) (cond
                                                                                                                 ((null? (filter (lambda (C) (not (null? (counter-queue C)))) slow-counters)) (serve (cdr requests) fast-counters slow-counters))
                                                                                                                 (else (serve (cdr requests) fast-counters (update (lambda (C) (remove-first-from-counter C)) slow-counters (car (min-et (filter (lambda (C) (not (null? (counter-queue C)))) slow-counters))))))))
                                  (else (cond
                                          ((null? (filter (lambda (C) (not (null? (counter-queue C)))) slow-counters)) (serve (cdr requests) (update (lambda (C) (remove-first-from-counter C)) fast-counters (car (min-et (filter (lambda (C) (not (null? (counter-queue C)))) fast-counters)))) slow-counters))
                                          (else (cond
                                                  ((<= (cdr (min-et (filter (lambda (C) (not (null? (counter-queue C)))) fast-counters))) (cdr (min-et (filter (lambda (C) (not (null? (counter-queue C)))) slow-counters))))
                                                   (serve (cdr requests) (update (lambda (C) (remove-first-from-counter C)) fast-counters (car (min-et (filter (lambda (C) (not (null? (counter-queue C)))) fast-counters)))) slow-counters))
                                                  (else (serve (cdr requests) fast-counters (update (lambda (C) (remove-first-from-counter C)) slow-counters (car (min-et (filter (lambda (C) (not (null? (counter-queue C)))) slow-counters)))))))))))]
                                                                                                                       
        )))

