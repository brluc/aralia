;;; alpha-sort.rkt
;;;
;;;

#lang racket

;(require rackunit)

(provide (all-defined-out))

;; Special character groups.
;;
;;         C 0067  I 0073  U 0085
;; A 0065  Ç 0199  Î 0206  Ù 0217
;; À 0192  E 0069  Ï 0207  Û 0219
;; Â 0194  È 0200          Ü 0220
;; Ä 0196  É 0201  O 0079
;; Æ 0198  Ê 0202  Ô 0212
;;         Ë 0203  Œ 0140

;; Reduce a diacritic symbol to its base form.
(define base-form-table
  (hash #\À #\A #\Â #\A #\Ä #\A
        #\Ç #\C
        #\È #\E #\É #\E #\Ê #\E #\Ë #\E
        #\Î #\I #\Ï #\I
        #\Ô #\O
        #\Ù #\U #\Û #\U #\Ü #\U))

;; The chapter to which an herbium entry belongs is
;; determined from the first letter of the plant name.
;; Special characters must be handled, diacritics
;; eliminated, and rare letters combined. This gives
;; us the chapter key. Chapter keys are used as keys
;; in hash tables and also as LaTeX chapter headings.
;;
;; First character of the name is upcased. If it is
;; not alphabetic, error. If it's in the base-form table,
;; replace it with its base form. If it's part of a group,
;; return the group key (I J K, X Y Z).
(define (get-chapter-key name)
  (let ((cc (char-upcase (string-ref (string-trim name) 0))))
    (if (char-alphabetic? cc)
        (let ((c (if (hash-has-key? base-form-table cc)
                 (hash-ref base-form-table cc)
                 cc)))
          (cond ((char=? #\Æ c) "A")
                ((char=? #\Œ c) "O")
                ((or (char=? #\I c)
                     (char=? #\J c)
                     (char=? #\K c)) "I J K")
                ((or (char=? #\X c)
                     (char=? #\Y c)
                     (char=? #\Z c)) "X Y Z")
                (else (format "~a" c))))
        (error "get-chapter-key: failed on name: " name))))

;; Expand ligatures.
;;
;;   Æ 0198 ==> AE
;;   Œ 0140 ==> OE
;;
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
(module+ test
  (require rackunit)
  (check-equal?
   (expand-ligatures "ŒÆÆŒ")
   "OEAEAEOE"
   "ligatures should be expanded properly"))

(define (eliminate-diacritics my-string)
  (list->string
   (map (lambda (c)
          (if (hash-has-key? base-form-table c)
              (hash-ref base-form-table c)
              c))
        (string->list my-string))))

(module+ test
  (check-equal?
   (eliminate-diacritics "AÀÂÄCÇEÈÉÊËIÎÏOÔUÙÛÜ")
   "AAAACCEEEEEIIIOOUUUU"
   "should eliminate diacritics and leave base forms")
  (check-equal?
   (eliminate-diacritics "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "should have no effect on base forms"))

;; Ignore non-alphanumeric characters.
(define (keep-alphabetic my-string)
  (list->string
   (filter char-alphabetic?
           (string->list my-string))))

(module+ test
  (check-equal?
   (keep-alphabetic "ŒÆÆŒÂÄ~*&CÇEÈÉBC D E _ - F123G;H:IJ@K0.Z")
   "ŒÆÆŒÂÄCÇEÈÉBCDEFGHIJKZ"
   "should keep only alphabetical characters"))

;; Do a diacritic-free ligature-free comparison,
;; ignoring non-alphabetic symbols and case.

;; Transform string into uppercase base forms.
;; Expanded ligatures, no diacritics.
(define (alpha-base-form my-string)
  (eliminate-diacritics
   (string-upcase
    (expand-ligatures
     (keep-alphabetic my-string)))))

(module+ test
  (check-equal?
   (alpha-base-form "Verge d’or à feuilles de graminées")
   "VERGEDORAFEUILLESDEGRAMINEES"
   "should keep alpha base-form characters only."))

;; Comparison function for use in sorting.
(define (alpha-base-form<? s1 s2)
  (string<? (alpha-base-form s1)
            (alpha-base-form s2)))

(module+ test
  (check-true
   (alpha-base-form<? "Verge d’or du Canada"
                      "Véronique à feuilles de thym")
   "should put Verge before Véronique")
  (check-true
   (alpha-base-form<? "Véronique à feuilles de thym"
                      "Violette du Labrador")
   "should put Véronique before Violette"))

;; Rows is a list of lists that are accessed by an
;; accessor function to build hash keys. Rows having
;; the same hash key are consed onto a list corresponding
;; to that key in the hash table. This is a classic hash
;; table method for classifying things. The hash table is
;; converted to a list and the list is returned. This list
;; is the chapter table.
(define (classify rows accessor get-hash-key)
  (let ((ht (make-hash)))
    (for-each
     (lambda (row)
       (let ((hk (get-hash-key (accessor row))))
         (if (hash-has-key? ht hk)
             (hash-update! ht hk (lambda (v) (cons row v)))
             (hash-set! ht hk (list row)))))
     rows)
    (hash->list ht)))

;; Sorts chapter table by chapter key.
(define (sort-chapter-keys chapters)
  (sort chapters string<? #:key car))

;; Sorts the entries in each chapter.
(define (alpha-sort-chapter-entries chapters)
  (map (lambda (chapter)
         (cons (car chapter)
               (sort (cdr chapter)
                     alpha-base-form<?
                     #:key car)))
       chapters))

;; Collects herbium entries (rows) into sorted chapters.
(define (collect-into-chapters rows)
  (sort-chapter-keys
   (alpha-sort-chapter-entries
    (classify rows car get-chapter-key))))



