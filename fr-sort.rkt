;; Portable French language string sort.
;; Racket's locale string comparison
;; works in Ubuntu but not in OSX.

#lang racket

(require rackunit)

(provide (all-defined-out))

;; May want to test our french string sorting
;; against racket's fr locale sorting. So set
;; the locale to French.
(current-locale "fr_CA.UTF-8")

;; We expand ligatures.
;;
;;   Æ 0198 ==> AE
;;   Œ 0140 ==> OE

(define (expand-ligatures my-string)
  (let loop ((cs (string->list my-string))
             (result empty))
    (cond ((null? cs)
           (list->string (reverse result)))
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
 (expand-ligatures "ŒÆÆŒ")
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

(define (eliminate-diacritics my-string)
  (list->string (map (lambda (c)
                       (if (hash-has-key? base-form-table c)
                           (hash-ref base-form-table c)
                           c))
                     (string->list my-string))))

(check-equal?
 (eliminate-diacritics "AÀÂÄCÇEÈÉÊËIÎÏOÔUÙÛÜ")
 "AAAACCEEEEEIIIOOUUUU"
 "should eliminate diacritics and leave base forms")

(check-equal?
 (eliminate-diacritics "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 "should have no effect on base forms")

;; Ignore non-alphanumeric characters.

(define (keep-alphabetic my-string)
  (list->string (filter char-alphabetic? (string->list my-string))))

(check-equal?
 (keep-alphabetic "ŒÆÆŒÂÄ~*&CÇEÈÉBC D E _ - F123G;H:IJ@K0.Z")
 "ŒÆÆŒÂÄCÇEÈÉBCDEFGHIJKZ"
 "should keep only alphabetical characters")

;; Do a diacritic-free ligature-free comparison,
;; ignoring non-alphabetic symbols.

(define (prepare-string my-string)
  (eliminate-diacritics
   (expand-ligatures
    (keep-alphabetic my-string))))

(define (string-base<? s1 s2)
  (string<? (prepare-string s1) (prepare-string s2)))

;; To do:
;;
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


