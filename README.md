CSV to SQL
  A small utility to convert any CSV file into a list of SQL queries. Practical to convert, say, a log, into a database fix.

## How does it work?

CSV-to-SQL queries the user for all relevant information about the CSV structure and SQL table to be updated, then converts the contents of the CSV file into a list of UPDATE SQL queries. One can then run the SQL query file against the target table to update all rows with the new data present in the CSV file.

## How to build

Racket 7.x or 8.x:
<pre>
raco exe csv-to-sql-queries.rkt
</pre>

## License
