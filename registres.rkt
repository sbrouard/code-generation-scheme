#lang racket

(require "./verif.rkt")


;; l: addlist with only elements smaller than n
;; Returns a list so that the first element is "n" and the sum of the two last elements equals "n"
(define (create-sumlist l n)  ;; '(1) 2 -> '(2 1 1)
  (cond [(null? l) (raise "ERROR : probably not an add-list")] 
        [(= n (+ (car l) (last l))) (list n (car l) (last l))]
        [(> n (+ (car l) (last l))) (create-sumlist (cdr l) n)]
        [(< n (+ (car l) (last l))) (create-sumlist (remove (last l) l) n)]
         ))


;; addlist-fin: entire addlist at the begining, then the first element is removed until it is null
;; addlist-deb: null at the begining, then elements which have been removed in addlist-fin are added in addlist-deb
;; l2: null at the begining
;; Returns the list "l2return" in which each element is a "create-sumlist"
(define (addlist2list-of-list addlist-fin addlist-deb l2return)
  (cond [(null? addlist-fin) l2return]
        [(= 1 (car addlist-fin)) (addlist2list-of-list (cdr addlist-fin) '(1) '((1 0 0)))]
        [else (addlist2list-of-list (cdr addlist-fin) (append addlist-deb 
                                                              (list (car addlist-fin))) 
                                    (append l2return 
                                            (list (create-sumlist addlist-deb (car addlist-fin)))))]))



; l = the end of an 'a-addlist'  ex : '((3 2 1) (5 3 2) (7 5 2))
; find if a variable is used again to compute x**n = if n is in l
; ex : (is-reused-variable? 2 '((3 2 1) (5 3 2))) -> #t
(define (is-reused-variable? n l)
  (cond [(null? l) #f]
        [(or (equal? n (caddr (car l))) (equal? n (cadr (car l)))) #t]
        [(null? (cdr l)) #f]
        [(or (equal? 'x (cadr l)) (number? (cadr l)) (equal? n (cadr (car l)))) #t]
        [else (is-reused-variable? n (cdr l))]))

(is-reused-variable? 2 '((3 2 1) (5 3 2)))
(addlist2list-of-list '(1 2 3) '() '())


; #f is 0 or 2 varaibles are reused in (car alist)
; #t else
(define (exactly-one-var-nonreused? alist)
  (or (and (not (is-reused-variable? (cadr (car alist)) (cdr alist)))

                  (is-reused-variable? (caddr (car alist)) (cdr alist)))
             (and (is-reused-variable? (cadr (car alist)) (cdr alist))
                  (not (is-reused-variable? (caddr (car alist)) (cdr alist))))

             (and (not (is-reused-variable? (cadr (car alist)) (cdr alist)))
                  (not (is-reused-variable? (caddr (car alist)) (cdr alist)))
                  (equal? (cadr (car alist)) (cdr alist)) (caddr (car alist)) (cdr alist))))


;(define (end-funct alist)
;  (cond [(null? alist) n]
;        [(null? (cdr alist)) (if (and (= 0 v)
;                                      (is-reused-variable? (cadr (car alist)) (cdr alist))
;                                      (is-reused-variable? (caddr (car alist)) (cdr alist)))
;                                 (add1 n)
;                                 n)]
;        [(and (= 0 (cadr (car alist)))
;              (= 0 (caddr (car alist))))
;         (nb-register (add1 n) v (cdr alist))])



;new-variable : name of the variable where we put n ex : x5
;alist  ex : '((1 0 0) (2 x1 x1) (3 x2 x1))
;ex : (modify-variable-name '((1 0 0) (2 x1 x1) (3 x2 x1) (6 3 3)) 3 'x2)
; -> '((1 0 0) (2 x1 x1) (3 x2 x1) (6 x2 x2))
(define (modify-variable-name alist n new-variable)
  (replace n new-variable alist))


; n = nb of register (=0 in first appeal)
; v = nb of variable that aren't used anymore (=0 in first appeal)
; l = addlist in the first appeal -> null
; attention !! n = -1 in first appeal
(define (nb-register n v alist)
  (cond [(null? alist) n]
        [(null? (cdr alist)) (if (and (= 0 v)
                                      (is-reused-variable? (cadr (car alist)) (cdr alist))
                                      (is-reused-variable? (caddr (car alist)) (cdr alist)))
                                 (add1 n)
                                 n)]
        [(and (= 0 (cadr (car alist)))
              (= 0 (caddr (car alist))))
         (nb-register (add1 n) v (cdr alist))]
        [(and (is-reused-variable? (cadr (car alist)) (cdr alist))
              (is-reused-variable? (caddr (car alist)) (cdr alist)))       
         (if (= 0 v)
             (nb-register (add1 n) v (cdr alist))
             (nb-register n (sub1 v) (cdr alist)))]
        [(exactly-one-var-nonreused? alist)
         (nb-register n v (cdr alist))]
        [else (nb-register n (add1 v) (cdr alist))]))

(nb-register -1 0 '((1 0 0) (2 1 1) (4 2 1) (5 4 1) (7 5 2)))
