# CSV to SQL

A small utility to convert any CSV file into a list of SQL queries. Practical to convert, say, a log, into a database fix.

## Version History

v1.1<br>
- In this version, the program asks for as many columns as there are values (plus the primary ID column).<br>
- If one enters 0, 2, 3, 4, the program will assume 0 is the primary ID column, and 2, 3 and 4 the value columns.<br>
- The SQL query is built with as many value sub-querie as necessary to accomodate.<br>
<br>
v1.0<br>
- Works for a primary ID and one value column.<br>
- Program asks user to name the delimiter, the target SQL table, the primary ID column, the SQL column to update, and the two column indexes for Primary ID and Value.<br>
- Program builds one SQL query per CSV line, and returns<br>
<br>
Conversion notes:<br>
- Double-quoted values are stripped off their double quotes<br>
- Doubled double-quotes in values are deduped;<br>
- Single quotes in values are doubled (for SQL compatibility).<br>

## Download

Download the latest release for Windows x64 [HERE](https://github.com/DexterLagan/csv-to-sql/releases).

## How does it work?

CSV-to-SQL queries the user for all relevant information about the CSV structure and SQL table to be updated, then converts the contents of the CSV file into a list of UPDATE SQL queries. One can then run the SQL query file against the target table to update all rows with the new data present in the CSV file.

## How to build

Racket 7.x or 8.x:
<pre>
raco exe csv-to-sql-queries.rkt
</pre>

## One-liner version

I included a [one-liner version](https://github.com/DexterLagan/csv-to-sql/blob/main/csv-to-sql-one-liner.rkt) of the same program, which uses static values, composition, threading and bypasses any input validation. The program is just one (long) line long, and was written to illustrate the weight of input validation in mission-critical, production software, as well as power of composition and threading:
<pre>
((comp_ (file->lines _)                                                                                                ; read file as lines
        (map (λ (line) (string-split line "|")) _)                                                                     ; split each line on |
        (map (λ (lst) (string-append "UPDATE parc SET status = '" (last lst) "' WHERE parc_id = " (first lst) ";")) _) ; build SQL query for each line
        (let ((content _) (file (put-file))) (display-lines-to-file content file #:exists 'replace)))                  ; save result in new file
 (get-file))                                                                                                           ; display file open dialog
</pre>

## What's next?

A few things would make this tool even more useful. For example:
- ability to configure the SQL query template;
- one-window GUI instead of a bunch of dialog prompts;
- generalizing line-splitting through an unlimited number of fields;
- integration with SQLite through its CSV import capabilities.

## License

CSV to SQL is free software; see [LICENSE](https://github.com/DexterLagan/csv-to-sql/blob/main/LICENSE) for more details.
