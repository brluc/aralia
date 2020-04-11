#lang racket

(require db)

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
              "create table plant (
                 uid integer primary key,
                 french_name text not null,
                 family text,
                 ref_victorin integer,
                 latin_name text,
                 index text,
                 edibility text
               );"))

(define (drop-herbium-table)
  (query-exec (conn) "drop table if exists plant"))

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
            "insert into publication
               (french_name,family,ref_victorin,latin_name,index,edibility)
             values 
               ($1,$2,$3,$4,$5,$6);"
            french-name family ref-victorin latin-name index edibility))))
      (plant-rows (read-herbium))))))





;; (define (fetch-ticket-args-by-code code)
;;   (query-maybe-row (conn)
;;                    "select title,cover_h,cover_w,color
;;                     from publication
;;                     where code=$1;" code))

;; (define (fetch-rows-by-title search-string)
;;   (map (lambda (row)
;;          (vector-map sql-null->false row))
;;        (query-rows (conn)
;;                    "select * from publication
;;                     where title like $1
;;                     order by title asc;"
;;                    (format "~a" search-string))))
