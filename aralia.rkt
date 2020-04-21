#lang racket

(define herbium-database-filename
  (make-parameter "herbium.rkt"))

(provide (all-defined-out))

(define (read-herbium)
  (call-with-input-file (herbium-database-filename) read))

;; Herbium entries are grouped alphabetically according
;; to the first character of name. The result is a list
;; of cons pairs: the character and then the group of entries.
;;
;; Name is the first element of each entry in herbium.
;; It must be a string for this to work. Not a list of
;; strings. So no alternate names.
(define (group-by-name rows)
  (let ((ht (make-hash)))
    (for-each
     (lambda (row) 
       (let ((ch (char-upcase (string-ref (car row) 0))))
         (if (hash-has-key? ht ch)
             (hash-update! ht ch (lambda (v)
                                   (cons row v)))
             (hash-set! ht ch '()))))
     rows)
    (sort (hash-map ht (lambda (k v)
                         (cons k (sort v #:key car string<?))))
          #:key car
          char<?)))

(define (document body)
  (string-append
   "\\input aralia-preamble\n"
   "\\begin{document}\n\n"
   "\\begin{titlingpage}\n"
   "\\maketitle\n"
   "\\end{titlingpage}\n"
   
   body

   "\n\n\\vfill\n\\end{document}\n"))

;; We frequently have to join lists of strings which
;; are lines of tex code.
(define (join-tex list-of-strings)
  (string-join list-of-strings "\n\n"))

;; Latex gets confused by #f in the database,
;; so we transform that to an empty string.
(define (false->blank u) (if u u ""))

;; For the case of one Latin name.
(define (tex-herbe name latin-name family ref comment)
  (format "\\herbe{~a}{~a}{~a}{~a}\n\n~a\n\n\\herbskip\n\n"
          name
          latin-name
          (string-titlecase family)
          ref
          comment))

;; For the case of two Latin names.
(define (tex-herbex name latin-name-1 latin-name-2 family ref comment)
  (format "\\herbex{~a}{~a}{~a}{~a}{~a}\n\n~a\n\n\\herbskip\n\n"
          name
          latin-name-1
          latin-name-2
          (string-titlecase family)
          ref
          comment))

;; Group herbium entries alphabetically.
;; Organize them into alphabetic chapters.
(define (make-body)
  (join-tex
   (map (lambda (alphabetic-group)
          (string-append
           (format "\\chapter*{~a}\n\n" (car alphabetic-group))
           (join-tex
            (map (lambda (row)
                   (match (map false->blank row)
                     ((list name family ref
                            (list latin-1 latin-2) comment edibility)
                      (tex-herbex name latin-1 latin-2 family ref comment))
                     ((list name family ref
                            latin comment edibility)
                      (tex-herbe name latin family ref comment))
                     (_ (error "make-body: match herbium entry failed: "
                               row))))
                 (cdr alphabetic-group)))))
        (group-by-name (read-herbium)))))




;; Final product.
(define (write-to-tex-file body)
  (call-with-output-file "aralia.tex" #:exists 'replace
    (lambda (out)
      (display
       (document body)
       out))))

(write-to-tex-file (make-body))

