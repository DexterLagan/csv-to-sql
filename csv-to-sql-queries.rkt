#lang racket/gui
(module+ test
  (require rackunit))

;;; purpose

; to process the given CSV file into an actionnable list of SQL queries

;;; consts

(define *appname*              "CSV-to-Any v1.0")
(define *default-filename*     "new-file")
(define *default-extension*    ".csv")

(define *column-index-msg*     "Please enter the two relevant column indexes to be used to build SQL queries:  \nExample: 0, 2")
(define *delimiter-msg*        "Please enter the column delimiter character:  ")
(define *table-msg*            "Please enter the table name:  ")
(define *primary-column-msg*   "Please enter the primary ID column name:  ")
(define *updated-column-msg*   "Please enter the name of the column to update:  ")
(define *file-save-msg*        "Please enter a file name to save to.  ")

(define *no-file-error*        "No file provided.  ")
(define *column-index-error*   "Missing or invalid column indexes.  \nIt has to be a list of two indexes.  ")
(define *column-delim-error*   "Missing or invalid column delimiter.  ")
(define *table-error*          "Missing or invalid table name.  ")
(define *primary-column-error* "Missing or invalid column name.  ")
(define *updated-column-error* "Missing or invalid updated column name")

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

;; returns an UPDATE SQL query given the different parameters
(define (get-sql-query table-name column-name value id-column id)
  (string-append "UPDATE " table-name " SET " column-name " = '" value "' WHERE " id-column " = " id ";"))
; unit test
(module+ test
  (check-equal? (get-sql-query "table" "column" "value" "id-column" "id")
                "UPDATE table SET column = 'value' WHERE id-column = id;"))

;; main processing proc
;; converts a line of CSV text into an SQL query
(define (process-csv line delimiter column-indexes table-name column-name id-column-name)
  (define field-lst  (string-split line delimiter))
  (define primary-id (list-ref field-lst (first  column-indexes)))
  (define value      (list-ref field-lst (second column-indexes)))
  (get-sql-query table-name column-name value id-column-name primary-id))
; unit test
(module+ test
  (check-equal? (process-csv "12345|some text here|new-status" "|" '(0 2) "parc" "status" "parc_id")
                "UPDATE parc SET status = 'new-status' WHERE parc_id = 12345;"))

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
      (die *no-file-error*)))

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
             (= 2 (length column-indexes)))
  (die *column-index-error*))

; get column delimiter from user
(define delimiter
  (get-text-from-user *appname* *delimiter-msg*))
(unless (and delimiter
             (= 1 (string-length delimiter)))
  (die *column-delim-error*))

; get SQL table name, column name and id column name from user
(define table-name     (get-string-or-die *table-msg*          *table-error*))
(define column-name    (get-string-or-die *primary-column-msg* *primary-column-error*))
(define id-column-name (get-string-or-die *updated-column-msg* *updated-column-error*))

; process csv and collect resulting SQL queries
(define result
  (map (λ (line)
         (process-csv line delimiter column-indexes table-name id-column-name column-name))
       file-lines))

; ask user to select an output filename
(define out-file
  (put-file *file-save-msg* #f #f *default-filename* *default-extension*))

; write result to file
(display-lines-to-file result out-file #:exists 'replace)


; EOF
