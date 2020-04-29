;; Portable French language string sort.
;; Racket's locale string comparison
;; works in Ubuntu but not in OSX.

#lang racket

(require rackunit)

;; May want to test our french string sorting
;; against racket's fr locale sorting. So set
;; the locale to French.
(current-locale "fr_CA.UTF-8")

;; We expand ligatures.
;;
;;   Æ 0198 ==> AE
;;   Œ 0140 ==> OE

(define (expand-ligatures cs)
  (let loop ((cs cs)
             (result empty))
    (cond ((null? cs)
           (reverse result))
          ((char-ci=? (car cs) #\Æ)
           (loop (cdr cs)
                 (cons #\E (cons #\A result))))
          ((char-ci=? (car cs) #\Œ)
           (loop (cdr cs)
                 (cons #\E (cons #\O result))))
          (else
           (loop (cdr cs)
                 (cons (car cs) result))))))

(check-equal?
 (list->string (expand-ligatures (string->list "ŒÆÆŒ")))
 "OEAEAEOE"
 "ligatures should be expanded properly")

;; Map out all the diacritic symbols into their
;; plain base forms. 

(define base-form-table
  (hash #\À #\A #\Â #\A #\Ä #\A
        #\Ç #\C
        #\È #\E #\É #\E #\Ê #\E #\Ë #\E
        #\Î #\I #\Ï #\I
        #\Ô #\O
        #\Ù #\U #\Û #\U #\Ü #\U))

(define (eliminate-diacritics cs)
  (map (lambda (c)
         (if (hash-has-key? base-form-table c)
             (hash-ref base-form-table c)
             c))
       cs))

(check-equal?
 (list->string
  (eliminate-diacritics
   (string->list "AÀÂÄCÇEÈÉÊËIÎÏOÔUÙÛÜ")))
 "AAAACCEEEEEIIIOOUUUU"
 "should eliminate diacritics and leave base forms")

(check-equal?
 (list->string
  (eliminate-diacritics
   (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 "should have no effect on base forms")

;; First do a regular diacritic-free ligature-free comparison.
;; If the two strings are different, we are done.
;; If they are the same, go to next step: comparison
;; of diacritic characters.

(define (base-form-sort cs)
  (eliminate-diacritics
   (expand-ligatures cs)))



;; Secondary ordering of diacritics:
;;
;;    vowel -- grave -- aigu -- circonflex -- trema
;;
;;    C -- C cedille



;; "AÀÂÄ"
;; "CÇ"
;; "EÈÉÊË"
;; "IÎÏ"
;; "OÔ"
;; "UÙÛÜ"


