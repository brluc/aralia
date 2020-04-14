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
   (format "\\documentclass[book,~apt,a4paper,onecolumn,openany]{memoir}\n"
           (font-size))
   (format "\\setlrmarginsandblock{~amm}{~amm}{*}\n"
           (left-margin)
           (right-margin))
   (format "\\setulmarginsandblock{~amm}{~amm}{*}\n"
           (upper-margin)
           (lower-margin))
   "\\fixthelayout\n"
   "\\tightlists"
   "\\usepackage{multirow}\n\n"
   "\\usepackage[utf8]{inputenc}\n"
   "\\usepackage[T1]{fontenc}\n"
   "\\usepackage[]{enumitem}\n"

   "\\title{Aralia Herbium}\n"
   "\\date{}\n"
      
   "\\begin{document}\n\n"
   "\\maketitle\n"
   
   body

   "\n\n\\vfill\n\\end{document}\n"))

(start)

(define (make-body)
  (string-join
   (map (lambda (row)
          (string-join
           (take (map (lambda (u)
                        (format "~a" (sql-null->false u)))
                      (vector->list row)) 3)
           " "))
        (query-rows (conn)
                    "select * from herbium order by french_name asc;"))
   "\n\n"))


(define (write-to-tex-file body)
  (call-with-output-file "aralia.tex" #:exists 'replace
    (lambda (out)
      (display
       (document (make-body))
       out))))

(write-to-tex-file "foo")
