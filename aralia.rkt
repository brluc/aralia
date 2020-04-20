#lang racket

(define herbium-database-filename (make-parameter "herbium2.rkt"))

(provide (all-defined-out))



;; (define (write-new-herbium)
  ;; (define (read-herbium)
  ;;   (call-with-input-file "herbium.rkt" read))
  ;; (call-with-output-file "herbium2.rkt"
  ;;   (lambda (out)
  ;;     (pretty-write
  ;;      (map vector->list (herbium-rows (read-herbium)))
  ;;      out))))






(define (document body)
  (string-append
   "\\input aralia-preamble\n"
   "\\begin{document}\n\n"
   "\\begin{titlingpage}\n"
   "\\maketitle\n"
   "\\end{titlingpage}\n"
   
   body

   "\n\n\\vfill\n\\end{document}\n"))

(define (make-body)
  (string-join
   (map (lambda (row)
          (match (map (lambda (u)
                        (if u (format "~a" u) ""))
                      row)
            ((list name family ref latin-name comment edibility)
             (format "\\herbe{~a}{~a}{~a}{~a}\n\n~a\n\n\\herbskip\n\n"
                     name
                     latin-name
                     (string-titlecase family)
                     ref
                     comment))
            (_ (error "make-body: match row failed: " row))))
        (call-with-input-file (herbium-database-filename) read))
   "\n\n"))


(define (write-to-tex-file body)
  (call-with-output-file "aralia.tex" #:exists 'replace
    (lambda (out)
      (display
       (document body)
       out))))

(write-to-tex-file (make-body))
