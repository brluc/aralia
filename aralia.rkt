#lang racket

(require db
         "dbms.rkt")

(provide (all-defined-out))

(define left-margin (make-parameter 15))
(define right-margin (make-parameter 15))
(define upper-margin (make-parameter 20))
(define lower-margin (make-parameter 20))
(define font-size (make-parameter 11))

(define (document body)
  (string-append
   "\\input aralia-preamble\n"
   "\\begin{document}\n\n"
   "\\begin{titlingpage}\n"
   "\\maketitle\n"
   "\\end{titlingpage}\n"
   
   body

   "\n\n\\vfill\n\\end{document}\n"))

(start)

(define (make-body)
  (string-join
   (map (lambda (row)
          (match (vector-map
                  (lambda (u)
                    (if (sql-null->false u)
                        (format "~a" u)
                        ""))
                  row)
            ((vector uid name family ref latin-name comment edibility)
             (format "\\herbe{~a}{~a}{~a}{~a}\n\n~a\n\n\\herbskip\n\n"
                     name
                     latin-name
                     (string-titlecase family)
                     ref
                     comment))
            (_ (error "make-body: match row failed: " row))))
        (query-rows (conn)
                    "select * from herbium order by french_name asc;"))
   "\n\n"))


(define (write-to-tex-file body)
  (call-with-output-file "aralia.tex" #:exists 'replace
    (lambda (out)
      (display
       (document body)
       out))))

(write-to-tex-file (make-body))
