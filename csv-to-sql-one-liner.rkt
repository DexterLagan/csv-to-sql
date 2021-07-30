#lang racket/gui
(require "comp_.rkt")

; purpose: one-liner version of csv-to-sql-queries using hard-wired values, composition, threading and no input checks.

((comp_ (file->lines _)                                                                                                ; read file as lines
        (map (λ (line) (string-split line "|")) _)                                                                     ; split each line on |
        (map (λ (lst) (string-append "UPDATE parc SET status = '" (last lst) "' WHERE parc_id = " (first lst) ";")) _) ; build SQL query for each line
        (let ((content _) (file (put-file))) (display-lines-to-file content file #:exists 'replace)))                  ; save result in new file
 (get-file))                                                                                                           ; display file open dialog


; EOF