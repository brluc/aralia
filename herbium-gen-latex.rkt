;;;
;;; herbium-gen-tex.rkt
;;;
;;; Generates latex herbium.tex from herbium.rkt database.
;;; herbium.tex contains all the typeset plant entries.
;;; It is then input into aralia.tex via
;;;
;;;   \input herbium.tex
;;;
;;;

#lang racket

(require "dict-sort.rkt")

(provide (all-defined-out))

(define herbium-database-filename
  (make-parameter "herbium.rkt"))

(define (read-herbium)
  (call-with-input-file (herbium-database-filename) read))

;; Install French language pack in Ubuntu:
;;
;;    sudo apt-get install language-pack-fr
;;
;;    fr_BE.UTF-8... done
;;    fr_CA.UTF-8... done  <= Canada
;;    fr_CH.UTF-8... done
;;    fr_FR.UTF-8... done  <= France
;;    fr_LU.UTF-8... done
;;
;; Set current locale to French-France for now.
;; Can set it to French-Canada if you like.

;; (current-locale "fr_CA.UTF-8")

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

;; Classify name under a chapter heading
;; by its first character. Left-trim the name
;; to be sure. Some chapters are grouped together,
;; e.g., IJK.
(define (get-chapter-key name)
  (let ((c (string-ref
            (string-locale-upcase
             (string-trim name)) 0)))
    (cond ((member c normal-group) (format "~a" c))
          ((member c A-group) "A")
          ((member c C-group) "C")
          ((member c E-group) "E")
          ((member c IJK-group) "I J K")
          ((member c O-group) "O")
          ((member c U-group) "U")
          ((member c XYZ-group) "X Y Z")
          (else
           (error "get-chapter-key: failed on name: " name)))))

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

;; We frequently have to join lists of strings which
;; are lines of tex code.
(define (join-tex list-of-strings)
  (string-join list-of-strings "\n\n"))

;; Latex gets confused by #f in the database,
;; so we transform that to an empty string.
(define (false->blank u) (if u u ""))

;; For the case of one Latin name.
(define (tex-herbe name latin-name family ref comment edibility)
  (format "\\begin{nonewpage}
\\herbe{~a}{~a}{~a}{~a}\n\n~a\\par\\noindent\\hfill {~a}
\\end{nonewpage}\n\n\\herbskip\n\n"
          name
          latin-name
          (string-titlecase family)
          ref
          comment
          (if (string=? "" edibility)
              ""
              (format "$\\bullet$ ~a" edibility))))

;; For the case of two Latin names.
(define (tex-herbex name latin-name-1 latin-name-2 family ref comment edibility)
  (format "\\begin{nonewpage}
\\herbex{~a}{~a}{~a}{~a}{~a}\n\n~a\\par\\noindent\\hfill {~a}
\\end{nonewpage}\n\n\\herbskip\n\n"
          name
          latin-name-1
          latin-name-2
          (string-titlecase family)
          ref
          comment
          (if (string=? "" edibility)
              ""
              (format "$\\bullet$ ~a" edibility))))

;; Group herbium entries alphabetically.
;; Organize them into alphabetic chapters.
;; car of chapter is chapter name. cdr of chapter
;; contains the plant entries.
(define (make-body)
  (join-tex
   (map (lambda (chapter)
          (string-append
           (format "\\chapter*{~a}\n\n" (car chapter))
           (join-tex
            (map (lambda (row)
                   (match (map false->blank row)
                     ((list name family ref
                            (list latin-1 latin-2) comment edibility)
                      (tex-herbex name latin-1 latin-2
                                  family ref comment edibility))
                     ((list name family ref
                            latin comment edibility)
                      (tex-herbe name latin family ref comment edibility))
                     (_ (error "make-body: cannot match row: " row))))
                 (cdr chapter)))))
        (collect-into-chapters (read-herbium)))))

;; Final product.
(define (write-to-tex-file body)
  (call-with-output-file "herbium.tex" #:exists 'replace
    (lambda (out)
      (display
       body
       out))))

(write-to-tex-file (make-body))


