;; Portable French language string sort.
;; Racket's locale string comparison
;; works in Ubuntu but not in OSX.

#lang racket

(require rackunit)

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

(define A-group (list #\A #\À #\Â #\Ä #\Æ))
(define C-group (list #\C #\Ç))
(define E-group (list #\E #\È #\É #\Ê #\Ë))
(define IJK-group (list #\I #\Î #\Ï #\J #\K))
(define O-group (list #\O #\Ô #\Œ))
(define U-group (list #\U #\Ù #\Û #\Ü))
(define XYZ-group (list #\X #\Y #\Z))
(define normal-group (list #\B #\D #\F #\G #\H #\L
                           #\M #\N #\P #\Q #\R #\S
                           #\T #\V #\W))



;;; need to test this


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


(define (eliminate-diacritics my-string)
  (list->string
   (map (lambda (c)
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
  (list->string
   (filter char-alphabetic?
           (string->list my-string))))

(check-equal?
 (keep-alphabetic "ŒÆÆŒÂÄ~*&CÇEÈÉBC D E _ - F123G;H:IJ@K0.Z")
 "ŒÆÆŒÂÄCÇEÈÉBCDEFGHIJKZ"
 "should keep only alphabetical characters")

;; Do a diacritic-free ligature-free comparison,
;; ignoring non-alphabetic symbols.

(define (prepare-string my-string)
  (eliminate-diacritics
   (string-upcase
    (expand-ligatures
     (keep-alphabetic my-string)))))

(define (string-base<? s1 s2)
  (string-ci<? (prepare-string s1)
               (prepare-string s2)))


(define (collect-into-chapters rows)
  (let ((hash-table (make-hash)))
    (for-each
     (lambda (row)
       (match row
         ((list name _ _ _ _ _)
          (let ((chapter-key (get-chapter-key name)))
            (if (hash-has-key? hash-table chapter-key)
                (hash-update! hash-table
                              chapter-key
                              (lambda (v)
                                (cons row v)))
                (hash-set! hash-table
                           chapter-key
                           (list row)))))
         (__ (error "collect-into-chapters: cannot match row: " row))))
     rows)
    (sort (hash-map hash-table
                    (lambda (k v)
                      (cons k (sort v
                                    #:key car
                                    string-base<?))))
          #:key car
          string-ci<?)))


