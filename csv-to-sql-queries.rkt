#lang racket/gui
(module+ test
  (require rackunit))

;;; purpose

; to process the given CSV file into an actionnable list of SQL queries

; Description of v1.0
; Works for a primary ID and one value column.
; Program asks user to name the delimiter, the target SQL table, the primary ID column, the SQL column to update, and the two column indexes for Primary ID and Value.
; Program builds one SQL query per CSV line, and returns

; Addendum for v1.1
; In this version, the program asks for as many columns as there are values (plus the primary ID column).
; If one enters 0, 2, 3, 4, the program will assume 0 is the primary ID column, and 2, 3 and 4 the value columns.
; The SQL query is built with as many value sub-querie as necessary to accomodate.

; Conversion notes:
; - double-quoted values are stripped off their double quotes
; - doubled double-quotes in values are deduped;
; - single quotes in values are doubled (for SQL compatibility)

;;; consts

(define *appname*              "CSV-to-SQL v1.1")
(define *default-filename*     "new-file")
(define *default-output-ext*   "txt")

(define *column-index-msg*     "Please enter the relevant column indexes to be used to build SQL queries:  \nExample: 0, 2, 3, 4")
(define *delimiter-msg*        "Please enter the column delimiter character:  ")
(define *table-msg*            "Please enter the table name:  ")
(define *primary-column-msg*   "Please enter the primary ID column name:  ")
(define *updated-column-msg*   "Please enter the name of the columns to update:    \nExample: name, address, code, other_column")
(define *file-save-msg*        "Please enter a file name to save to.  ")

(define *column-index-error*   "Missing or invalid column indexes.  \nIt has to be a list of indexes.  ")
(define *column-delim-error*   "Missing or invalid column delimiter.  ")
(define *table-error*          "Missing or invalid table name.  ")
(define *primary-column-error* "Missing or invalid primary ID column name.  ")
(define *updated-column-error* "Missing or invalid updated column names")

;;; version history

; v1.1 - supports unlimited number of SQL columns / values to update at once.
; v1.0 - initial release. 

;;; defs

;; displays an error dialog
(define show-error-message
  (λ args
    (message-box *appname* (apply ~a args) #f '(ok stop))))

;; returns the first command line argument if one was present, #f otherwise.
(define (get-first-arg)
  (define args (current-command-line-arguments))
  (if (> (vector-length args) 0)
      (vector-ref args 0)
      #f))

;; displays an error message before quitting
(define die
  (λ args
    (show-error-message (apply ~a args))
    (exit 1)))

;; returns a file, given a path on the command line, or from an open dialog otherwise.
;; returns #f if none provided
(define (get-file-from-args-or-dialog)
  (define first-arg (get-first-arg))
  (if first-arg first-arg
      (get-file)))

;; asks the user for two column indexes
(define (get-column-indexes-from-user)
  (get-text-from-user *appname* *column-index-msg*))

; returns a function that composes parameters in order,
; using a placeholder _ for passing values between functions.
(define-syntax (comp_ stx)
  ; macro to compose functions passing an '_' parameter
  (syntax-case stx ()
    ((_ f1 ...)
     (with-syntax ([x-var (datum->syntax stx '_)])
       #'(apply compose1 (reverse (list (λ (x-var) f1) ...)))))))

;; returns a list of numbers, given a string containing a list of numbers
;; returns #f otherwise
(define (string->list-of-numbers str)
  (let/cc return
    ((comp_ (string-split _ ",")
            (if (or (null? _)
                    (< (length _) 2))
                (return #f)
                (map string-trim _))
            (map string->number _)
            (if (andmap number? _)
                _
                #f))
     str)))
; unit test
(module+ test
  (check-equal? (string->list-of-numbers "0,2") '(0 2))
  (check-equal? (string->list-of-numbers "1, 2, 3") '(1 2 3))
  (check-equal? (string->list-of-numbers "1,") #f)
  (check-equal? (string->list-of-numbers ",2") #f)
  (check-equal? (string->list-of-numbers "hello") #f)
  (check-equal? (string->list-of-numbers "a,b") #f)
  (check-equal? (string->list-of-numbers "1,a") #f)
  (check-equal? (string->list-of-numbers "b,2") #f))

;; returns a list of strings, given a string containing a list of words or sentences
;; if a single string is given, a 1-item list will be returned
;; returns #f otherwise
(define (string->list-of-strings str)
  (let/cc return
    ((comp_ (string-split _ ",")      ; -> '("test")
            (if (or (null? _)
                    (< (length _) 1))
                (return #f)
                (map string-trim _))
            (map ~a _))
     str)))
; unit test
(module+ test
  (check-equal? (string->list-of-strings "test-string") '("test-string"))
  (check-equal? (string->list-of-strings "0,2") '("0" "2"))
  (check-equal? (string->list-of-strings "1, 2, 3") '("1" "2" "3"))
  (check-equal? (string->list-of-strings "1,") '("1"))
  (check-equal? (string->list-of-strings ",2") '("2"))
  (check-equal? (string->list-of-strings "hello") '("hello"))
  (check-equal? (string->list-of-strings "a,b") '("a" "b"))
  (check-equal? (string->list-of-strings "1,a") '("1" "a"))
  (check-equal? (string->list-of-strings "b,2") '("b" "2")))

;; returns an UPDATE SQL query given the different parameters
(define (get-sql-query table-name column-names values id-column id)
  ; helper func to build column-value pairs string
  ; strips double-quotes, and dedupe double-quotes inside values
  (define (make-column-value column-name value)
    (define clean-value
      (if (and (string-prefix? value "\"")
               (string-suffix? value "\""))
          (string-replace (substring value 1 (- (string-length value) 1))
                          "\"\""
                          "\"")
          value))
    (string-append column-name " = '" (string-replace clean-value "'" "''") "'"))
  ; build each part
  (define header         (string-append "UPDATE " table-name " SET "))
  (define columns-values (string-join (map make-column-value column-names values) ", "))
  (define footer         (string-append " WHERE " id-column " = " id ";"))
  ; put it all together
  (string-append header columns-values footer))
; unit test
(module+ test
  ; test single column query
  (check-equal? (get-sql-query "table" '("column") '("value") "id-column" "id")
                "UPDATE table SET column = 'value' WHERE id-column = id;")
  ; test quote in value
  (check-equal? (get-sql-query "table" '("column") '("d'value") "id-column" "id")
                "UPDATE table SET column = 'd''value' WHERE id-column = id;")
  ; test multiple column query
  (check-equal? (get-sql-query "table" '("column1" "column2" "column3") '("value1" "value2" "value3") "id-column" "id")
                "UPDATE table SET column1 = 'value1', column2 = 'value2', column3 = 'value3' WHERE id-column = id;")
  ; test quoted value
  (check-equal? (get-sql-query "table" '("column1" "column2" "column3") '("value1" "\"value2\"" "value3") "id-column" "id")
                "UPDATE table SET column1 = 'value1', column2 = 'value2', column3 = 'value3' WHERE id-column = id;")
  ; test quoted value with quote
  (check-equal? (get-sql-query "table" '("column1" "column2" "column3") '("value1" "\"va \"\" lue2\"" "value3") "id-column" "id")
                "UPDATE table SET column1 = 'value1', column2 = 'va \" lue2', column3 = 'value3' WHERE id-column = id;"))

;; main processing proc
;; converts a line of CSV text into an SQL query
(define (process-csv line delimiter column-indexes table-name column-names id-column-name)
  (define min-fields-length (length column-indexes))
  (define field-lst (map string-trim (string-split line delimiter #:trim? #f))) ; "12345|some text here|new-status" -> ("12345" "some text here" ...
  (if (>= (length field-lst) min-fields-length)                                 ; if number of fields matches index list count
      (let ((primary-id (list-ref field-lst (car column-indexes)))
            (values     (map (curry list-ref field-lst) (cdr column-indexes))))
        (get-sql-query table-name column-names values id-column-name primary-id))
      #f))
; unit test
(module+ test
  (check-equal? (process-csv "12345|some text here|new-status"
                             "|"
                             '(0 2)
                             "parc"
                             '("status")
                             "parc_id")
                "UPDATE parc SET status = 'new-status' WHERE parc_id = 12345;")
  (check-equal? (process-csv "12345|some text here|new-status|other-column-value"
                             "|"
                             '(0 2 3)
                             "parc"
                             '("status" "other-column")
                             "parc_id")
                "UPDATE parc SET status = 'new-status', other-column = 'other-column-value' WHERE parc_id = 12345;")
  (check-equal? (process-csv "12345, some text here, new-status, other-column-value, another-column-value"
                             ","
                             '(0 2 3 4)
                             "parc"
                             '("status" "other-column" "yet-another-column-to-update")
                             "parc_id")
                "UPDATE parc SET status = 'new-status', other-column = 'other-column-value', yet-another-column-to-update = 'another-column-value' WHERE parc_id = 12345;")
  ; test double-quotes handling
  (check-equal? (process-csv "12345, \"some \"\"text\"\" here\", new-status, other-column-value, l'another-column-value"
                             ","
                             '(0 1 2 3 4)
                             "parc"
                             '("quoted-thing" "status" "other-column" "yet-another-column-to-update")
                             "parc_id")
                "UPDATE parc SET quoted-thing = 'some \"text\" here', status = 'new-status', other-column = 'other-column-value', yet-another-column-to-update = 'l''another-column-value' WHERE parc_id = 12345;")
  ; test full fields handling
  (check-equal? (process-csv "489, Accessoires (Type), Haut-parleur,Processeur (Marque),Intel,Processeur (Modele),i5"
                             ","
                             '(0 1 2 3 4 5 6)
                             "propriete_valeur"
                             '("caracteristique_odoo_1" "valeur_odoo_1" "caracteristique_odoo_2" "valeur_odoo_2" "caracteristique_odoo_3" "valeur_odoo_3")
                             "propriete_valeur_num")
                "UPDATE propriete_valeur SET caracteristique_odoo_1 = 'Accessoires (Type)', valeur_odoo_1 = 'Haut-parleur', caracteristique_odoo_2 = 'Processeur (Marque)', valeur_odoo_2 = 'Intel', caracteristique_odoo_3 = 'Processeur (Modele)', valeur_odoo_3 = 'i5' WHERE propriete_valeur_num = 489;")
  ; test empty fields handling
  (check-equal? (process-csv "489, Accessoires (Type), Haut-parleur,,,,"
                             ","
                             '(0 1 2 3 4 5 6)
                             "propriete_valeur"
                             '("caracteristique_odoo_1" "valeur_odoo_1" "caracteristique_odoo_2" "valeur_odoo_2" "caracteristique_odoo_3" "valeur_odoo_3")
                             "propriete_valeur_num")
                "UPDATE propriete_valeur SET caracteristique_odoo_1 = 'Accessoires (Type)', valeur_odoo_1 = 'Haut-parleur', caracteristique_odoo_2 = '', valeur_odoo_2 = '', caracteristique_odoo_3 = '', valeur_odoo_3 = '' WHERE propriete_valeur_num = 489;"))

;; ask user to enter a string, quit with the given error message if none provided
(define (get-string-or-die msg error)
  (define input
    (get-text-from-user *appname* msg))
  (unless (non-empty-string? input)
    (die error))
  (string-trim input))

;;; main

; gather file from the command line, or from an open dialog.
(define file (get-file-from-args-or-dialog))

; read file contents as lines or quit
(define file-lines
  (if file
      (file->lines file)
      (exit 0)))

; get revelant column index string or quit
(define column-indexes-str
  (get-column-indexes-from-user))
(unless (and column-indexes-str
             (string-contains? column-indexes-str ","))
  (die *column-index-error*))

; convert column index string into a list of numbers or quit
(define column-indexes
  (string->list-of-numbers column-indexes-str))
(unless (and column-indexes
             (>= (length column-indexes) 2))
  (die *column-index-error*))

; get column delimiter from user
(define delimiter
  (get-text-from-user *appname* *delimiter-msg*))
(unless (and delimiter
             (= 1 (string-length delimiter)))
  (die *column-delim-error*))

; get SQL table name, column name and id column name from user
(define table-name          (get-string-or-die *table-msg*          *table-error*))
(define column-name         (get-string-or-die *primary-column-msg* *primary-column-error*))
(define target-columns-str  (get-string-or-die *updated-column-msg* *updated-column-error*))
(define target-column-names (if (non-empty-string? target-columns-str)
                                (string->list-of-strings target-columns-str)
                                #f))
(unless target-column-names
  (die *column-index-error*))

; process csv and collect resulting SQL queries
(define result
  (map (λ (line)
         (process-csv line delimiter column-indexes table-name target-column-names column-name))
       file-lines))

; ask user to select an output filename
(define out-file
  (put-file *file-save-msg* #f #f *default-filename* *default-output-ext*))

; write result to file
(display-lines-to-file result out-file #:exists 'replace)


; EOF
