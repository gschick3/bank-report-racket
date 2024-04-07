#lang typed/racket

; Helper functions

; Unpack list of Options, throwing out all False
; Typed Racket requires explicit testing to remove Option label.
; Filtering #f values out isn't satisfactory, so I've created my own function using conditionals
(: filter-options (All (A) (-> (Listof (Option A)) (Listof A))))
(define (filter-options lst)
  (: loop (-> (Listof (Option A)) (Listof A) (Listof A)))
  (define (loop old new)
    (cond
      [(empty? old) new]
      [(first old) (loop (rest old) (append new (list (first old))))]
      [else (loop (rest old) new)]))
  (loop lst '()))


; Types and Constructors

; Account
(struct Account ([number : Positive-Integer] [name : String] [balance : Flonum]))

(: strings->account (-> String String String (Option Account)))
(define (strings->account number name balance)
  (let ([num (string->number number)]
        [bal (string->number balance)])
    (if (and (exact-positive-integer? num) (flonum? bal))
        (Account num name bal)
        #f)))

; Purchase
(define-type Purchase (List 'purchase Positive-Integer Positive-Integer String Positive-Flonum))

(: mk-purchase (-> Positive-Integer Positive-Integer String Positive-Flonum Purchase))
(define (mk-purchase account-number timestamp merchant amount)
  (list 'purchase account-number timestamp merchant amount))

(: strings->purchase (-> String String String String (Option Purchase)))
(define (strings->purchase account-number timestamp merchant amount)
  (let ([num (string->number account-number)]
        [ts  (string->number timestamp)]
        [amt (string->number amount)])
    (if (and (exact-positive-integer? num) (exact-positive-integer? ts) (flonum? amt) (positive? amt))
        (mk-purchase num ts merchant amt)
        #f)))

; Payment
(: mk-cash-payment (-> Positive-Integer Positive-Integer Positive-Flonum (List 'payment Positive-Integer Positive-Integer 'cash Positive-Flonum)))
(define (mk-cash-payment account-number timestamp amount)
  (list 'payment account-number timestamp 'cash amount))

(: mk-check-payment (-> Positive-Integer Positive-Integer Positive-Integer Positive-Flonum (List 'payment Positive-Integer Positive-Integer 'check Positive-Integer Positive-Flonum)))
(define (mk-check-payment account-number timestamp check-number amount)
  (list 'payment account-number timestamp 'check check-number amount))

(define-type Card-Type (U 'credit 'debit))

(: mk-card-payment (-> Positive-Integer Positive-Integer Card-Type Positive-Integer Positive-Flonum (List 'payment Positive-Integer Positive-Integer Card-Type Positive-Integer Positive-Flonum)))
(define (mk-card-payment account-number timestamp type card-number amount)
  (list 'payment account-number timestamp type card-number amount))

(define (strings->transaction [lst : (Listof String)]) : (Option (U Purchase (Listof String)))
  (cond 
    [(equal? (first lst) "Purchase") (strings->purchase (second lst) (third lst) (fourth lst) (fifth lst))]
    [(equal? (first lst) "Payment") (list "payments")]
    [else (list "none of the above")]))

; Reading files

; Split line on all spaces while keeping quoted strings together
(define (split-line [line : String]) : (Listof String)
  (regexp-match* (pregexp "(?<=\")[^\\s][^\"]+[^\\s](?=\")|[\\w.]+") line))

(define (read-accounts [filename : String]) : (Listof Account)
  (filter-options (map (lambda ([l : String])
                         (let ([line : (Listof String) (split-line l)])
                           (strings->account (first line) (second line) (third line))))
                       (file->lines filename))))

(define (read-transactions [filename : String]) : (Listof (U Purchase (Listof String)))
  (filter-options (map (lambda ([l : String])
                         (strings->transaction (split-line l)))
                       (file->lines filename))))