#lang racket

(require "dbms.rkt")

(start)

(define (fetch-all)
  (map (lambda (row) (vector-map sql-null->false row))
       (query-rows (conn)
                   "select * from herbium order by french_name asc")))

(displayln
 (format-table/default
  (map (lambda (row)
         (take (vector->list row) 4))
       (fetch-all))))

