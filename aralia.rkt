#lang racket

(define herbium-database-filename
  (make-parameter "herbium.rkt"))

(provide (all-defined-out))

(define (document body)
  (string-append
   "\\input aralia-preamble\n"
   "\\begin{document}\n\n"
   "\\begin{titlingpage}\n"
   "\\maketitle\n"
   "\\end{titlingpage}\n"
   
   body

   "\n\n\\vfill\n\\end{document}\n"))

;; Latex gets confused by #f in the database,
;; so we transform that to an empty string.
(define (false->blank u) (if u u ""))

(define (tex-herbe name latin-name family ref comment)
  (format "\\herbe{~a}{~a}{~a}{~a}\n\n~a\n\n\\herbskip\n\n"
          name
          latin-name
          (string-titlecase family)
          ref
          comment))

(define (tex-herbex name latin-name-1 latin-name-2 family ref comment)
  (format "\\herbex{~a}{~a}{~a}{~a}{~a}\n\n~a\n\n\\herbskip\n\n"
          name
          latin-name-1
          latin-name-2
          (string-titlecase family)
          ref
          comment))

(define (make-body)
  (string-join
   (map (lambda (row)
          (match (map false->blank row)
            ((list name family ref (list latin-1 latin-2) comment edibility)
             (tex-herbex name latin-1 latin-2 family ref comment))
            ((list name family ref latin comment edibility)
             (tex-herbe name latin family ref comment))
            (_ (error "make-body: match herbium entry failed: " row))))
        (call-with-input-file (herbium-database-filename) read))
   "\n\n"))

(define (write-to-tex-file body)
  (call-with-output-file "aralia.tex" #:exists 'replace
    (lambda (out)
      (display
       (document body)
       out))))

(write-to-tex-file (make-body))
