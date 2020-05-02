;;; main.rkt
;;;
;;; User interface. Use this module to debug
;;; and hack around with herbium.

#lang racket

(require "alpha-sort.rkt")

(provide (all-defined-out))

(define herbium-database-filename
  (make-parameter "herbium.rkt"))

(define (read-herbium)
  (call-with-input-file (herbium-database-filename) read))

(define herbium (read-herbium))

(define (find-latin-pairs)
  (filter
   (lambda (row)
     (match row
       ((list _ _ _ latin _ _ )
        (list? latin))))
   herbium))
