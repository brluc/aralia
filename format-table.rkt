;;; format-table.rkt
;;;
;;; Functions for displaying column-aligned
;;; plain-text tables.

#lang racket

(require racket/format)  ;; ~a, ~r

(provide format-real
         format-table
         format-table/default)

;; A "table" is a list of lists of strings.

(define (transpose table)
  (apply map list table))

(define (get-max-string-length string-list)
  (apply max (map string-length string-list)))

(define (get-max-column-widths table)
  (map get-max-string-length (transpose table)))

(define (format-field field width align)
  (~a #:width width #:align align field))

(define (format-real x precision)
  (~r x #:precision (list '= precision)))

;; If we need it:
;;
;; (~r 1024 #:base 2 #:min-width 32 #:pad-string "0")
;; ==> "00000000000000000000010000000000"
;;
;; (~r pi #:precision (list '= 5))
;; ==> "3.14159

;; if align is empty then aligments default to right.
(define (normalize table align)
  (let ((max-column-widths (get-max-column-widths table))
        (row-length (length (car table))))
    (map (lambda (row) 
           (map format-field row 
                max-column-widths 
                (if (empty? align)
                    (make-list row-length 'right)
                    align)))
         table)))

(define (joinln list-of-strings)
  (string-join list-of-strings "\n"))

(define (format-table table
                      #:separator [separator " | "]
                      #:align [align empty]
                      #:header-char [header-char #f])  
  (let ((normalized-table (normalize table align)))
    (if header-char
        (let* ((header (string-join (first normalized-table)
                                    (make-string (string-length separator) 
                                                 #\Space)))
               (header-underline (make-string (string-length header) 
                                              header-char)))
          (joinln (cons header 
                        (cons header-underline
                              (map (lambda (row)
                                     (string-join row separator)) 
                                   (rest normalized-table))))))
        (joinln (map (lambda (row) 
                       (string-join row separator))
                     normalized-table)))))

;; Automatically changes cells into strings,
;; with reasonable defaults. #t and #f are hard
;; to read, we print them as True and False.
(define (format-table/default #:header (header #f)
                              #:flonum-precision (flonum-precision 5)
                              body)
  (define (format-default cell)
    (cond ((flonum? cell)
           (format-real cell flonum-precision))
          ((boolean? cell)
           (if cell "True" "False"))
          (else
           (format "~a" cell))))
  (if header
      (format-table #:separator " | "
                    #:header-char #\-
                    (cons (map format-default header)
                          (map (lambda (row)
                                 (map format-default row))
                               body)))
      (format-table #:separator " | "
                    (map (lambda (row)
                           (map format-default row))
                         body))))


;; ==================================================
;;
;; For testing:

(define tt '(("zarf" "greeblies" "terwilligbert")
             ("1" "2" "3")
             ("10" "20" "30")
             ("100" "200" "300")))

(define tt2 (list (list 1.34219823 pi (log pi 2))
                  (list 'zarf 'arthur "merlin")
                  (list 91/53 #f #t)
                  (list '(1 2 3) "foo" 3.919-2.8734i)))

;; > (displayln (format-table/default tt2))

;; 1.34220 | 3.14159 |       1.65150
;;    zarf |  arthur |        merlin
;;   91/53 |      #f |            #t
;; (1 2 3) |     foo | 3.919-2.8734i

;; > (displayln (format-table/default tt2 #:flonum-precision 2))

;;    1.34 |   3.14 |          1.65
;;    zarf | arthur |        merlin
;;   91/53 |     #f |            #t
;; (1 2 3) |    foo | 3.919-2.8734i

;; > (displayln (format-table/default
;;               tt2 #:header '("yabba" "dabba" "doo")))
;;
;;   yabba     dabba             doo
;; ---------------------------------
;; 1.34220 | 3.14159 |       1.65150
;;    zarf |  arthur |        merlin
;;   91/53 |      #f |            #t
;; (1 2 3) |     foo | 3.919-2.8734i

;; (displayln (format-table tt #:separator " | " #:header-char #\=))
;; zarf   greeblies   terwilligbert
;; ================================
;;    1 |         2 |             3
;;   10 |        20 |            30
;;  100 |       200 |           300

;; (displayln (format-table tt
;;                          #:align '(left left left)
;;                          #:separator " | "
;;                          #:header-char #\-))
;; zarf   greeblies   terwilligbert
;; --------------------------------
;; 1    | 2         | 3            
;; 10   | 20        | 30           
;; 100  | 200       | 300          

