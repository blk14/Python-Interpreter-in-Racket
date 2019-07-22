#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)

 (require racket/trace)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) empty-stack)

(define (push value stack) (cons value stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (cons stack (cons co-varnames (cons co-consts (cons co-names (cons co-code (cons IC '())))))))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (cadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (caddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (cadddr stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (car (cddddr stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)  (car (cdr (cddddr stack-machine))))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (let iter ([mySymbols symbols]
             [symbol symbol]
             [result 0])
             (if (equal? symbol (car mySymbols))
                 result
                 (iter (cdr mySymbols) symbol (+ result 1)))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (let iter ([item item]
             [symbol symbol]
             [stack-machine stack-machine]
             [cur_iteration 0])
    (if (eq? cur_iteration 5)
        (if (eq? (get-symbol-index symbol) 5)
            (cons item '())
            (cons (top stack-machine) '()))
        (if (eq? (get-symbol-index symbol) cur_iteration)
            (cons item (iter item symbol (pop stack-machine) (+ cur_iteration 1)))
            (cons (top stack-machine) (iter item symbol (pop stack-machine) (+ cur_iteration 1)))))))


;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (push (value stack-machine)))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (pop stack-machine))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

(define (get-elem-from-pos i mylist cur_pos)  
  (if (equal? i cur_pos) (car mylist)
      (get-elem-from-pos i (cdr mylist) (+ cur_pos 1))))

(define (add-on-pos i list elem)
   (hash-set list i elem))

(define (multy-update items symbols stack-machine)     ; vreau sa fac update-uri la mai multe liste din stack-machine
  (if (equal? (length items) 1)
      (update-stack-machine (car items) (car symbols) stack-machine)
      (multy-update (cdr items) (cdr symbols) (update-stack-machine (car items) (car symbols) stack-machine))))

(define dict-compare-operation (hash 0 (λ(a b) (if (< a b) #t #f))
                                     1 (λ (a b) (if (<= a b) #t #f))
                                     2 (λ (a b) (if (equal? a b) #t #f))
                                     3 (λ (a b) (if (not (equal? a b)) #t #f))
                                     4 (λ (a b) (if (> a b) #t #f))
                                     5 (λ (a b) (if (>= a b) #t #f))
                                     6 (λ (a b) (if (eq? a b) #t #f))
                                     7 (λ (a b) (if (not (eq? a b)) #t #f))))


(define functions (hash "print" (λ (a) (write (car a)))
                        "range" (λ (a) (range (car a)))
                        "sqrt" (λ (a) (sqrt (car a)))
                        "prod" (λ (l) (foldl * 1 l))))


(define (ntop n stack)
  (if (equal? n 0)
      '()
      (cons (car stack) (npop (- n 1) (cdr stack)))))

(define (npop n stack)
  (if (equal? n 0)
      stack
      (npop (- n 1) (cdr stack))))
                               
      

(define (run-stack-machine stack-machine)
  (if (null? (get-code stack-machine))
      stack-machine
      (cond
        ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'LOAD_CONST) ; se verifica pt LOAD_CONST daca este pe pozitia IC in sm
         (run-stack-machine (multy-update 
          (list (push (hash-ref (get-consts stack-machine)
                                (cdr (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)))   ;pun pe stiva lui stack-machine elem de la pozitia
                      (get-stack stack-machine))                                                    ;data de al 2 lea param al elem din co_code
            (+ (get-IC stack-machine) 1))
          (list'STACK 'INSTRUCTION-COUNTER) stack-machine)))
                                                                                            
        ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'LOAD_FAST)
         (run-stack-machine (multy-update                            ;reapelez actualizand stiva din stack-machine
         (list (push (hash-ref (get-varnames stack-machine)
                               (cdr (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)))
                     (get-stack stack-machine))
               (+ (get-IC stack-machine) 1))
         (list'STACK 'INSTRUCTION-COUNTER) stack-machine)))
        
        ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'STORE_FAST)
         (run-stack-machine (multy-update
            (list (pop (get-stack stack-machine))                       ;pt multy update pun in lista items 
                  (hash-set (get-varnames stack-machine)        ;stiva cu pop, map-ul varnames cu update
                            (cdr (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0))        
                            (top (get-stack stack-machine)))
                  (+ (get-IC stack-machine) 1))                       ; si IC
            
                            (list 'STACK 'CO-VARNAMES 'INSTRUCTION-COUNTER) stack-machine)))       ; setez simbolurile

        ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'BINARY_ADD)
         (run-stack-machine (multy-update
              (list (push
                     (+ (car (get-stack stack-machine)) (cadr (get-stack stack-machine))) (cddr (get-stack stack-machine)))
                    ;fac operatia si pun rezultatul pe stiva fara cele 2 elemente
                    
                    (+ (get-IC stack-machine) 1))
              (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))       ; setez simbolurile
        
        ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'INPLACE_ADD)
         (run-stack-machine (multy-update
              (list (push
                     (+ (car (get-stack stack-machine)) (cadr (get-stack stack-machine))) (cddr (get-stack stack-machine)))
                    ;fac operatia si pun rezultatul pe stiva fara cele 2 elemente
                    
                    (+ (get-IC stack-machine) 1))
              (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))       ; setez simbolurile
        
        
        ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'BINARY_SUBTRACT)
         (run-stack-machine (multy-update
              (list (push
                     (- (cadr (get-stack stack-machine)) (car (get-stack stack-machine))) (cddr (get-stack stack-machine)))
                    ;fac operatia si pun rezultatul pe stiva fara cele 2 elemente
                    
                    (+ (get-IC stack-machine) 1))
              (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))       ; setez simbolurile

         ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'INPLACE_SUBTRACT)
         (run-stack-machine (multy-update
              (list (push
                     (- (cadr (get-stack stack-machine)) (car (get-stack stack-machine))) (cddr (get-stack stack-machine)))
                    ;fac operatia si pun rezultatul pe stiva fara cele 2 elemente
                    
                    (+ (get-IC stack-machine) 1))
              (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))       ; setez simbolurile

         

          ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'BINARY_MODULO)
         (run-stack-machine (multy-update
              (list (push
                     (modulo (cadr (get-stack stack-machine)) (car (get-stack stack-machine))) (cddr (get-stack stack-machine)))
                    ;fac operatia si pun rezultatul pe stiva fara cele 2 elemente
                    
                    (+ (get-IC stack-machine) 1))
              (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))       ; setez simbolurile

           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'INPLACE_MODULO)
         (run-stack-machine (multy-update
              (list (push
                     (modulo (cadr (get-stack stack-machine)) (car (get-stack stack-machine))) (cddr (get-stack stack-machine)))
                    ;fac operatia si pun rezultatul pe stiva fara cele 2 elemente
                    
                    (+ (get-IC stack-machine) 1))
              (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))       ; setez simbolurile

          ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'COMPARE_OP)
           (run-stack-machine (multy-update
               (list (push ((hash-ref dict-compare-operation
                                     (cdr (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)))
                            (cadr (get-stack stack-machine)) (car (get-stack stack-machine))) (cddr (get-stack stack-machine)))
                     (+ (get-IC stack-machine) 1))
               ;iau din dictionarul dict-compare-operatio operatia de la pozitia data de argumentul lui COMPARE_OP
               ;si ii dau si argumentele operatieie care e o functie lambda, astfel obtinand rezultatul functiei
               ; care e pus pe o stiva fara cele 2 argumente ale functiei lambda, apoi incrementez IC

               (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))

          ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'POP_JUMP_IF_FALSE)
           (run-stack-machine (multy-update
                   (list (pop (get-stack stack-machine))           ; scot elementul din lista
                         (if (equal? (car (get-stack stack-machine)) #f)
                             (/ (cdr (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 2)
                             (+ (get-IC stack-machine) 1)))
                          ; daca elementul este fals atunci sar la pozitia indicata de argumentul comenzii
                          ;altfel incrementez IC
                   (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))

           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'POP_JUMP_IF_TRUE)
           (run-stack-machine (multy-update
                   (list (pop (get-stack stack-machine))           ; scot elementul din lista
                         (if (equal? (car (get-stack stack-machine)) #t)
                             (/ (cdr (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 2)
                             (+ (get-IC stack-machine) 1)))
                          ; daca elementul este fals atunci sar la pozitia indicata de precedenta comanada (COMPARE_OP)
                          ;altfel incrementez IC
                   (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))

           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'FOR_ITER)
            (run-stack-machine
             (if (null? (top (get-stack stack-machine)))             ; verific daca TOS este null
                                   (multy-update (list (pop (get-stack stack-machine))
                                                       (+ (get-IC stack-machine)
                                                          (+ (/ (cdr (get-elem-from-pos (get-IC stack-machine)
                                                                                        (get-code stack-machine) 0)) 2) 1)))
                                                 (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)
                                   ;daca este null atunci apelez multy update modificand stiva cu pop
                                   ;iar la IC adun delta pe 2 + 1 (delta fiind cdr de la instructiunea curenta)
                                   
                                   (multy-update (list (push (car (car (get-stack stack-machine)))
                                                             (push (cdr (car (get-stack stack-machine)))
                                                                   (pop (get-stack stack-machine))) )
                                                       (+ (get-IC stack-machine) 1))
                                                 (list 'STACK 'INSTRUCTION-COUNTER) stack-machine))))
                                  ; daca nu e null modific de 3 ori stiva cu un pop, push (cdr TOS) si push (car TOS)
                                   ;*update: fac push de car de tos pe o stiva modificata cu un push de cdr de TOS pe
                                  ; o stiva modificata cu un pop de TOS

           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'JUMP_ABSOLUTE)
            (run-stack-machine (multy-update
                                (list (/ (cdr (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 2))
                                (list 'INSTRUCTION-COUNTER)
                                stack-machine)))

           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'SETUP_LOOP)
            (run-stack-machine (multy-update (list (+ (get-IC stack-machine) 1))
                                             (list 'INSTRUCTION-COUNTER) stack-machine)))
           
           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'POP_BLOCK)
            (run-stack-machine (multy-update (list (+ (get-IC stack-machine) 1))
                                             (list 'INSTRUCTION-COUNTER) stack-machine)))
           
           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'GET_ITER)
            (run-stack-machine (multy-update (list (+ (get-IC stack-machine) 1))
                                             (list 'INSTRUCTION-COUNTER) stack-machine)))

           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'LOAD_GLOBAL)
            (run-stack-machine (multy-update (list (push (hash-ref (get-names stack-machine)
                                                         
                                                                    (cdr (get-elem-from-pos (get-IC stack-machine)
                                                                                  (get-code stack-machine)
                                                                                  0))) stack-machine)
                                                           
                                                   (+ (get-IC stack-machine) 1))
                                             (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))
           ;iau elementul de la pozitia data de argumentul (cdr) instructiunii curente din lista names

           

           ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'CALL_FUNCTION)
            (run-stack-machine (multy-update
                      (list (push ((hash-ref functions
                                            (car (npop (cdr (get-elem-from-pos (get-IC stack-machine)
                                                                               (get-code stack-machine) 0))
                                                       (get-stack stack-machine))))
                                            (ntop (cdr (get-elem-from-pos (get-IC stack-machine)
                                                                               (get-code stack-machine) 0))
                                                  (get-stack stack-machine)))
                                  (npop (+ (cdr (get-elem-from-pos (get-IC stack-machine)
                                                                               (get-code stack-machine) 0)) 1)
                                        (get-stack stack-machine)))
                            (+ (get-IC stack-machine) 1))
                      (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))


           ;((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'POP_TOP)
            ;(run-stack-machine (multy-update (list
             ;                                 (pop (get-stack stack-machine))
              ;                                (+ (get-IC stack-machine) 1))
               ;                              (list 'STACK 'INSTRUCTION-COUNTER) stack-machine)))
                                                  
                    
        ((equal? (car (get-elem-from-pos (get-IC stack-machine) (get-code stack-machine) 0)) 'RETURN-VALUE)
         stack-machine)
        
        (else stack-machine))))

(trace run-stack-machine)


(run-stack-machine
	(make-stack-machine 
		empty-stack 
		#hash((0 . None) (1 . None) (2 . None) (3 . None) (4 . None) (5 . None) (6 . None) (7 . None) (8 . None))
		#hash((0 . 0) (1 . 9) (2 . 16) (3 . 6) (4 . 10) (5 . 100) (6 . 81) (7 . 36) (8 . 15))
		#hash((0 . "sqrt"))
		'((LOAD_CONST . 1) (STORE_FAST . 0) (LOAD_CONST . 2) (STORE_FAST . 1) (LOAD_GLOBAL . 0) (LOAD_FAST . 0) (CALL_FUNCTION . 1) (STORE_FAST . 2) (LOAD_GLOBAL . 0) (LOAD_FAST . 1) (CALL_FUNCTION . 1) (STORE_FAST . 3) (LOAD_FAST . 2) (LOAD_FAST . 3) (BINARY_ADD . 0) (STORE_FAST . 4) (LOAD_CONST . 8) (STORE_FAST . 5) (LOAD_FAST . 5) (LOAD_CONST . 4) (BINARY_ADD . 0) (STORE_FAST . 5) (LOAD_GLOBAL . 0) (LOAD_FAST . 5) (CALL_FUNCTION . 1) (STORE_FAST . 6) (LOAD_FAST . 4) (LOAD_FAST . 6) (INPLACE_ADD . 0) (STORE_FAST . 4) (LOAD_FAST . 4) (LOAD_GLOBAL . 0) (LOAD_CONST . 5) (CALL_FUNCTION . 1) (INPLACE_ADD . 0) (STORE_FAST . 4) (LOAD_GLOBAL . 0) (LOAD_CONST . 6) (CALL_FUNCTION . 1) (LOAD_GLOBAL . 0) (LOAD_CONST . 7) (CALL_FUNCTION . 1) (BINARY_ADD . 0) (STORE_FAST . 7) (LOAD_FAST . 7) (LOAD_CONST . 4) (INPLACE_ADD . 0) (STORE_FAST . 7) (LOAD_GLOBAL . 0) (LOAD_FAST . 7) (CALL_FUNCTION . 1) (STORE_FAST . 8) (LOAD_CONST . 0) (RETURN_VALUE . 0))
		0))