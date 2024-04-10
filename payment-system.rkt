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
; String constructor
(define (strings->account number name balance)
  (let ([num (string->number number)]
        [bal (string->number balance)])
    (if (and (exact-positive-integer? num) (flonum? bal))
        (Account num name bal)
        #f)))

; Purchase
(define-type Purchase (List 'purchase Positive-Integer Positive-Integer String Positive-Flonum))

(: make-purchase (-> Positive-Integer Positive-Integer String Positive-Flonum Purchase))
; Basic constructor
(define (make-purchase account-number timestamp merchant amount)
  (list 'purchase account-number timestamp merchant amount))

(: strings->purchase (-> String String String String (Option Purchase)))
; String constructor
(define (strings->purchase account-number timestamp merchant amount)
  (let ([num (string->number account-number)]
        [ts  (string->number timestamp)]
        [amt (string->number amount)])
    ; requires checks to confirm that types are correct
    (if (and (exact-positive-integer? num) (exact-positive-integer? ts) (flonum? amt) (positive? amt))
        (make-purchase num ts merchant amt)
        #f)))

; Payment
(define-type Payment (U Cash-Payment Check-Payment Card-Payment))
(define-type Cash-Payment (List 'payment Positive-Integer Positive-Integer 'cash Positive-Flonum))
(define-type Check-Payment (List 'payment Positive-Integer Positive-Integer 'check Positive-Integer Positive-Flonum))
(define-type Card-Payment (List 'payment Positive-Integer Positive-Integer Card-Type Positive-Integer Positive-Flonum))

(define-type Card-Type (U 'credit 'debit))

(: make-cash-payment (-> Positive-Integer Positive-Integer Positive-Flonum Cash-Payment))
; Basic constructor
(define (make-cash-payment account-number timestamp amount)
  (list 'payment account-number timestamp 'cash amount))

(: strings->cash-payment (-> String String String (Option Cash-Payment)))
; String constructor
(define (strings->cash-payment account-number timestamp amount)
  (let ([num (string->number account-number)]
        [ts  (string->number timestamp)]
        [amt (string->number amount)])
    (if (and (exact-positive-integer? num) (exact-positive-integer? ts) (flonum? amt) (positive? amt))
        (make-cash-payment num ts amt)
        #f)))

(: make-check-payment (-> Positive-Integer Positive-Integer Positive-Integer Positive-Flonum Check-Payment))
; Basic constructor
(define (make-check-payment account-number timestamp check-number amount)
  (list 'payment account-number timestamp 'check check-number amount))

(: strings->check-payment (-> String String String String (Option Check-Payment)))
; String constructor
(define (strings->check-payment account-number timestamp check-number amount)
  (let ([num (string->number account-number)]
        [ts  (string->number timestamp)]
        [chk (string->number check-number)]
        [amt (string->number amount)])
    (if (and (exact-positive-integer? num) (exact-positive-integer? ts) (exact-positive-integer? chk) (flonum? amt) (positive? amt))
        (make-check-payment num ts chk amt)
        #f)))

(: make-card-payment (-> Positive-Integer Positive-Integer Card-Type Positive-Integer Positive-Flonum Card-Payment))
; Basic constructor
(define (make-card-payment account-number timestamp type card-number amount)
  (list 'payment account-number timestamp type card-number amount))

(: strings->card-payment (-> String String String String Card-Type (Option Card-Payment)))
; String constructor
(define (strings->card-payment account-number timestamp card-number amount type)
  (let ([num (string->number account-number)]
        [ts  (string->number timestamp)]
        [crd (string->number card-number)]
        [amt (string->number amount)])
    (if (and (exact-positive-integer? num) (exact-positive-integer? ts) (exact-positive-integer? crd) (flonum? amt) (positive? amt))
        (make-card-payment num ts type crd amt)
        #f)))

; Transaction
(define-type Transaction (U Purchase Payment))

(: strings->transaction (-> (Listof String) (Option Transaction)))
; String constructor
(define (strings->transaction lst)
  (cond
    [(< (length lst) 5) #f]
    [(string-ci=? (first lst) "purchase")
     (strings->purchase (second lst) (third lst) (fourth lst) (fifth lst))]
    [(string-ci=? (first lst) "payment")
     (cond
       [(string-ci=? (fourth lst) "cash")
        (strings->cash-payment (second lst) (third lst) (fifth lst))]
       [(and (string-ci=? (fourth lst) "check") (>= (length lst) 6))
        (strings->check-payment (second lst) (third lst) (fifth lst) (sixth lst))]
       [(string-ci=? (fourth lst) "credit") (strings->card-payment (second lst) (third lst) (fifth lst) (sixth lst) 'credit)]
       [(string-ci=? (fourth lst) "debit") (strings->card-payment (second lst) (third lst) (fifth lst) (sixth lst) 'debit)]
       [else #f])]
    [else #f]))


; Reading files

; Split line on all spaces while keeping quoted strings together
(: split-line (-> String (Listof String)))
(define (split-line line)
  (regexp-match* (pregexp "(?<=\")[^\\s][^\"]+[^\\s](?=\")|[\\w.]+") line))

; Read account information from file
(: read-accounts (-> String (Listof Account)))
(define (read-accounts filename)
  (filter-options (map (lambda ([l : String])
                         (let ([line : (Listof String) (split-line l)])
                           (strings->account (first line) (second line) (third line))))
                       (file->lines filename))))

; Read transaction information from file
(: read-transactions (-> String (Listof Transaction)))
(define (read-transactions filename)
  (filter-options (map (lambda ([l : String])
                         (strings->transaction (split-line l)))
                       (file->lines filename))))


; Processing transactions

; Get all transactions for given account
(: get-account-transactions (-> Account (Listof Transaction) (Listof Transaction)))
(define (get-account-transactions account transactions)
  (filter (lambda ([transaction : Transaction])
            (equal? (Account-number account) (second transaction)))
          transactions))

; Fold all transactions to total purchases and payments
(: total-transactions (-> (Listof Transaction) (List Flonum Flonum)))
(define (total-transactions transactions)
  (foldl (lambda ([transaction : Transaction] [stats : (List Flonum Flonum)])
           (let ([amt (last transaction)])
             (cond
               [(not (flonum? amt)) stats] ; Type checker needs confirmation
               [(equal? (first transaction) 'purchase) (list (+ (first stats) amt) (second stats))]
               [else (list (first stats) (+ (second stats) amt))])))
         '(0.0 0.0)
         transactions))


; Output
(define col-width 10)

; Write everything to file
(: write-all (-> (Listof Account) (Listof Transaction) String Void))
(define (write-all accounts transactions path)
  (display-to-file (all->string accounts transactions) path #:exists 'replace))

; Turn everything into a string
(: all->string (-> (Listof Account) (Listof Transaction) String))
(define (all->string accounts transactions)
  (string-join (map (lambda ([account : Account])
                      (let ([account-transactions (get-account-transactions account transactions)])
                        (format "STATEMENT OF ACCOUNT\n~a\n\n~a\n\n~a\n\n~a"
                                (account->string account)
                                (string-join (map transaction->string account-transactions) "\n")
                                (summary->string account account-transactions)
                                (~a #:width 50 #:pad-string "*"))))
                    accounts)
               "\n"))

; Get string of account header
(: account->string (-> Account String))
(define (account->string account)
  (~a (~a (Account-number account) #:min-width 20)
      (~a (Account-name account) #:min-width 20)
      (~a "Starting Balance: " (~r (Account-balance account) #:precision `(= 2)) #:min-width 20)))

; Get string of transaction
(: transaction->string (-> Transaction String))
(define (transaction->string transaction)
  (cond
    [(equal? (first transaction) 'purchase)
     (~a (~a (third transaction) #:min-width col-width)
         (~a "Purchase" #:min-width col-width)
         (~a (fourth transaction) #:min-width 20 #:max-width 40)
         (~r (fifth transaction) #:min-width col-width #:precision '(= 2)))]
    [else
     (~a (~a (third transaction) #:min-width col-width)
         (~a "Payment" #:min-width col-width)
         (string-titlecase (~a (fourth transaction) #:min-width 20))
         (~r (let ([amt (last transaction)]) (if (flonum? amt) amt 0.0)) #:min-width col-width #:precision '(= 2)))]))

; Get string of account summary
(: summary->string (-> Account (Listof Transaction) String))
(define (summary->string account transactions)
  (let ([summary (total-transactions transactions)])
    (format "Total Purchases:~a\nTotal Payments: ~a\nEnding Balance: ~a"
            (~r (first summary) #:min-width col-width #:precision '(= 2))
            (~r (second summary) #:min-width col-width #:precision '(= 2))
            (~r (+ (Account-balance account) (- (first summary) (second summary))) #:min-width col-width #:precision '(= 2)))))


; Driver
(: generate-report (-> String String String Void))
(define (generate-report accounts-path transactions-path output-path)
  (write-all (read-accounts accounts-path) (read-transactions transactions-path) output-path))



(generate-report "ACCOUNTS.TXT" "TRANSACTIONS.TXT" "STATEMENTS.TXT")