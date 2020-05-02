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

(require "alpha-sort.rkt")

(provide (all-defined-out))

;; (define herbium-database-filename
;;   (make-parameter "herbium.rkt"))

;; (define (read-herbium)
;;   (call-with-input-file (herbium-database-filename) read))

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

;; Group herbium entries alphabetically.
;; Organize them into alphabetic chapters.
;; car of chapter is chapter name. cdr of chapter
;; contains the plant entries.
(define (make-body db)
  (join-tex
    (map (lambda (chapter)
           (string-append
            (format "\\chapter*{~a}\n\n" (car chapter))
            (join-tex
             (map (lambda (row)
                    (match (map false->blank row)
                      ((list name family ref latin comment edibility)
                       (tex-herbe name latin family ref comment edibility))
                      (_ (error "make-body: cannot match row: " row))))
                  (cdr chapter)))))
         (collect-into-chapters db))))



