;;;
;;; dbms.rkt
;;;


#lang racket

(require db
         "format-table.rkt")

(provide (all-defined-out))

(define herbium-database-filename
  (make-parameter "herbium.rkt"))

(struct herbium (schema rows) #:prefab)

(define (read-herbium)
  (call-with-input-file (herbium-database-filename) read))

(define conn (make-parameter #f))

(define (connect)
  (conn (sqlite3-connect #:database 'memory)))

(define (create-herbium-table)
  (query-exec (conn)
              "create table herbium (
                 uid integer primary key,
                 french_name text not null,
                 family text,
                 ref_victorin integer,
                 latin_name text,
                 idx text,
                 edibility text
               );"))

(define (drop-herbium-table)
  (query-exec (conn) "drop table if exists herbium"))

(define (populate-herbium-table)
  (call-with-transaction
   (conn)
   (lambda ()
     (for-each
      (lambda (row)
        (match (vector-map false->sql-null row)
          ((vector french-name family ref-victorin
                   latin-name index edibility)
           (query-exec
            (conn)
            "insert into herbium
               (french_name,family,ref_victorin,latin_name,idx,edibility)
             values 
               ($1,$2,$3,$4,$5,$6);"
            french-name family ref-victorin latin-name index edibility))))
      (herbium-rows (read-herbium))))))

(define (start)
  (if (conn)
      (begin (drop-herbium-table)
             (create-herbium-table)
             (populate-herbium-table))
      (begin (connect)
             (create-herbium-table)
             (populate-herbium-table))))




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

