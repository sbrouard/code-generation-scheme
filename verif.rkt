#lang racket

(provide is-growing?)
(provide is-sum?)
(provide all-are-sum?)
(provide is-addlist?)

(provide replace)
(provide replace-list-of-list)
(provide create-list-of-list)
(provide result)
(provide stock)
(provide sum-of)
(provide delete-greater)
(provide exponentiation-stock)
(provide exponentiation-init)



;; --------------Verif----------------------



;; Returns #t if l is a strictly increasing list or null; #f otherwise
;; l must be a list of "int"
(define (is-growing? l) 
  (cond [(null? l) #t]
        [(null? (cdr l)) #t]
        [(= (car l) (cadr l)) #f]
        [(> (car l) (cadr l)) #f]
        [else (is-growing? (cdr l))]))



;; Returns #t if x is the sum of 2 elements of the list l ; #f otherwise
;;
(define (is-sum? l x)
  (cond [(null? l) #f]
        [(= x (+ (car l) (last l))) #t]
        [(< x (+ (car l) (last l))) (is-sum? (remove (last l) l) x)]
        [(> x (+ (car l) (last l))) (is-sum? (cdr l) x)]
        ))


;; Returns #t if all elements of l2 are sum of 2 elts of l1; #f otherwise
;; l1 must be composed of (car l) and l2 of (crd l) in order to test if l is an add-list
(define (all-are-sum? l1 l2)
  (cond [(null? l2) #t]
        [(null? l1) #f]
        [(not (is-sum? l1 (car l2))) #f]
        [else (all-are-sum? (append l1 (list (car l2))) (cdr l2))]))



;; General fonction of verification :
;; Returns #t if l is an addlist ; #f otherwise
(define (is-addlist? l)
  (cond [(null? l) #f]
        [(not (= 1 (car l))) #f]
        [(not (is-growing? l)) #f]
        [else (all-are-sum? (list (car l)) (cdr l))]))







;; ------------------- exponentiation -------------------



;; Replaces in the list "l" the element "elt" by "new"
;; ex : (replace 2 42 '(1 2 3)) -> '(1 42 3)
(define (replace elt new l)
  (cond [(null? l) (raise "ERROR : elt is not in the list")]
        [(list? (car l)) (cons (car l) (replace elt new (cdr l)))]
        [(= elt (car l)) (cons new (cdr l))]
        [else (cons (car l) (replace elt new (cdr l)))]))


;; "l" must be an alist (list of lists), and "elt" a list in "l"
;; Replaces "elt" by "new" in "l"
;; ex : (replace-list-of-list '(2) 42 '((1) (2) (3))) -> '((1) 42 (3))
(define (replace-list-of-list elt new l) 
  (cond [(null? l) (raise "ERROR : elt is not in the list")]
        [(= (car elt) (car (car l))) (cons new (cdr l))]
        [else (cons (car l) (replace-list-of-list elt new (cdr l)))]))


; l and lcomp must be the same list in the begining
; Replaces each element "elt" of "l" by "(list elt)"
; ex : (create-list-of-list '(1 2 3) '(1 2 3)) -> '((1) (2) (3))
(define (create-list-of-list l lcomp) 
  (if (null? l)
      lcomp
      (create-list-of-list (cdr l) (replace (car l) (list (car l)) lcomp))))





;; Returns the result of x**n if it has already been computed, -1 otherwise
;; The result of x**n is stocked in "l" after n (in the same list as n)
;; ex: (result 2 '((1) (2) (3))) -> -1
;; ex: (result 2 '((1) (2 3) (3))) -> 3
(define (result n lcomplete)
  (cond [(null? lcomplete) (raise "ERROR : n is not in l")]
        [(= (car (car lcomplete)) n)
         (if (null? (cdr (car lcomplete)))
          -1
          (car (cdr (car lcomplete))))]
        [else (result n (cdr lcomplete))]))




;; In the list "l", stocks "res" after "elt" in the list that begins by "elt"
;; ex : (stock 42 2 '((1) (2) (3))) -> '((1) (2 42) (3))
(define (stock res elt lcomplete)
  (cond [(null? lcomplete) (raise "ERROR : elt is not in l")]
        [else (replace-list-of-list (list elt) (cons elt (list res)) lcomplete)]))


        

;; Returns a list of two elements of l which sum gives x
(define (sum-of l x)  
  (cond [(null? l) (raise "ERROR")]
        [(= x (+ (car l) (last l))) (list (car l) (last l))]
        [(< x (+ (car l) (last l))) (sum-of (remove (last l) l) x)]
        [(> x (+ (car l) (last l))) (sum-of (cdr l) x)]
        ))


;; Deletes all elements greater than x in l
;; x must be in l, and l must be growing
(define (delete-greater x l)  
  (cond [(null? l) l]
        [(= (last l) x) l]
        [else (delete-greater x (remove x l <))]))


;; l must be an addlist
;; lcomplete is the alist created by (create-list-of-list l l)
;; Computes x**(last l) and stocks each results in lcomplete
(define (exponentiation-stock x l lcomplete) 
  (define n (result (last l) lcomplete))
  (cond [(not (is-addlist? l)) (raise "ERROR : require an add-list")]
        [(not (= -1 n)) n]
        [(null? (cdr l)) x]
        [else 
         (let ([p (sum-of l (last l))])
           (* (cond [(= 0 (car p)) 1]
                    [(= 1 (car p)) x]
                    [else (let ([res (exponentiation-stock x (delete-greater (car p) l) lcomplete)])
                            (stock res (last l) lcomplete) res)])
              (cond [(= 1 (cadr p)) x]
                    [else (let ([res (exponentiation-stock x (delete-greater (cadr p) l) lcomplete)])
                            (stock res (last l) lcomplete) res)])))]))


;; Computes x**(last l)
;; ex: (exponentiation-init 3 '(1 2 3)) -> 27 (3³)
(define (exponentiation-init x l)
  (let ([ll (create-list-of-list l l)])
    (exponentiation-stock x l ll)))





;; ---------exponentiation  (naif)--------------------


(define (exponentiation x l) 
  (cond [(not (is-addlist? l)) (raise "ERROR : require an add-list")]
        [(null? (cdr l)) x]
        [else 
         (let ([p (sum-of l (last l))])
           (* (cond [(= 1 (car p)) x]
                    [else (exponentiation x (delete-greater (car p) l))])
              (cond [(= 1 (cadr p)) x]
                    [(exponentiation x (delete-greater (cadr p) l))])))]))

;(exponentiation 3 '(1 2 3)) ;; 3³=27 





;;;;;;;;;;;;;;;;;;;;;;  Unused functions (for exponentiation) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (sum-of l x)  ;; return two elements of l which sum gives x
;  (cond [(null? l) (raise "ERROR")]
;        ;[(null? (cdr l)) (list 0 (car l))] ;; ligne a supprimer (a verifier)
;        [(= x (+ (car l) (last l))) (list (car l) (last l))]
;        [(< x (+ (car l) (last l))) (sum-of (remove (last l) l) x)]
;        [(> x (+ (car l) (last l))) (sum-of (cdr l) x)]
;        ))


;(define (sum-of-list-of-list l x)  ;; return two elements of l which sum gives x
;  (cond [(null? l) (raise "ERROR")]
;        ;[(null? (cdr l)) (list 0 (car l))] ;; ligne a supprimer (a verifier)
;        [(= x (+ (car (car l)) (car (last l)))) (list (car (car l)) (car (last l)))]
;        [(< x (+ (car (car l)) (car (last l)))) (sum-of-list-of-list (remove (last l) l) x)]
;        [(> x (+ (car (car l)) (car (last l)))) (sum-of-list-of-list (cdr l) x)]
;        ))


;(define (delete-greater-list-of-list x l) ; deletes all elements greater than x in l 
;  (cond [(null? l) l]
;        [(= (car (last l)) x) l] ; l must be growing
;        [else (delete-greater x (remove x l <))]))
;
