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

(define (expand-ligatures my-string)
  (let loop ((cs (string->list my-string))
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
 (expand-ligatures "ŒÆÆŒ")
 '(#\O #\E #\A #\E #\A #\E #\O #\E)
 "ligatures should be expanded properly")

;; First do a regular diacritic-free comparison.
;; If the two strings are different, we are done.
;; If they are the same, go to next step: comparison
;; of diacritic characters.







;; Secondary ordering of accents.
;;
;;    vowel -- grave -- aigu -- circonflex -- trema
;;

;; "AÀÂÄ"
;; "CÇ"
;; "EÈÉÊË"
;; "IÎÏ"
;; "OÔ"
;; "UÙÛÜ"


