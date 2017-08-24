# SQLite3 Native API for Emacs 25+
sqlite-napi is a dynamic module for GNU Emacs that provides 
direct access to the SQLite3 C API.
## Usage
~~~el
(require 'sqlite3-napi)

(let ((dbh)
      (stmt))
  (unwind-protect
      (progn
	(setq dbh (sqlite3-open "/path/to/db"
				(logior sqlite-open-readwrite
					sqlite-open-create)))
	(setq stmt (sqlite3-prepare dbh "select * from some_table"))
	(while (= sqlite-row (sqlite3-step stmt))
	  (cl-loop for i from 0 to (1- (sqlite3-column-count stmt)) do
		   (message "Column #%d: %s => %s" i
			    (sqlite3-column-name stmt i)
			    (sqlite3-column-text stmt i)))))
    ;; Clean up
    (sqlite3-finalize stmt)
    (sqlite3-close dbh)))
~~~
This is alpha software and might crash your Emacs. Save your work before
trying it out.

## Requirements
- Emacs 25.1 or above, compiled with module support (`--with-modules`)
- sqlite3 library and header file
- C compiler
## Installation
~~~sh
$ git co https://github.com/pekingduck/sqlite3-napi
$ cd sqlite3-napi
$ make
$ cp sqlite3-napi.el sqlite3-napi-module.so /your/elisp/load-path/
~~~
Once the code stabilizes I will make it available on melpa.
## Constants
These constants (defined in sqlite3.h) are things such as numeric result codes from various interfaces (ex: `SQLITE_OK`) or flags passed into functions to control behavior (ex: `SQLITE_OPEN_READONLY`).

In elisp they are in lowercase and words are separated by "-" instead of
"_". For example, `SQLITE_OK` would be `sqlite-ok`.

Refer to the [offical site](https://www.sqlite.org/rescode.html) 
for a full list of constants. 
## Functions
Here I will briefly describe functions available in this module.

Consult the [offical API documentation](https://www.sqlite.org/c3ref/funclist.html) for details.
### sqlite3-open
~~~el
(sqlite3-open "/path/to/data-file" flags)
~~~
Open the database file and return a database handle.

In case of error, the function raises `'db-error` along with a
corresponding error message.

### sqlite3-close
~~~el
(sqlite3-close database-handle)
~~~
Close the database file.
### sqlite3-prepare
~~~el
(sqlite3-prepare database-handle sql-statement)
~~~
Compile the supplied SQL statement and return a statement handle.
### sqlite3-finalize
~~~el
(sqlite3-finalize statement-handle)
~~~
Destroy a prepared statement.
### sqlite3-step
~~~el
(sqlite3-step statement-handle)
~~~
Execute a prepared SQL statement. Some of the return codes are:

`sqlite-done` - the statement has finished executing successfully.

`sqlite-row` - if the SQL statement being executed returns any data, then `sqlite-row` is returned each time a new row of data is ready for processing by the caller. 

### sqlite3-changes
~~~el
(sqlite3-changes database-handle)
~~~
Return the number of rows modified (for update/delete/insert statements)

### sqlite3-reset
~~~el
(sqlite3-reset statement-handle)
~~~
Reset a prepared statement. Call this function if you want to re-bind 
the statement to new variables.
### sqlite3-last-insert-rowid
~~~el
(sqlite3-last-insert-rowid database-handle)
~~~
Retrieve the last inserted rowid (64 bit). 

Notes: Beware that Emacs only supports integers up to 61 bits.
### sqlite3-get-autocommit
~~~el
(sqlite3-get-autocommit database-handle)
~~~
Return 1 / 0 if auto-commit mode is ON / OFF.
### sqlite3-exec
~~~el
(sqlite3-exec database-handle sql-statements &optional callback)
~~~
The Swiss Army Knife of the API, you can execute multiple SQL statements
(separated by ";") in a row with just one call.

The callback function, if supplied, is invoked for *each row* and should accept 3
 parameters: 
 1. the first parameter is the number of columns in the current row;
 2. the second parameter is the actual data (as a list strings); 
 3. the third one is a list of column names. 
 
To signal an error condition inside the callback, return `nil`. 
`sqlite3_exec()` will stop the execution and return `sqlite-abort`.

An example of a callback:
~~~el
(defun print-row (ncols data names)
  (cl-loop for i from 0 to (1- ncols) do
           (message "[%d]%s->%s" elt (ncols names i) (elt data i))
           (message "--------------------"))
  t)
  
(sqlite3-exec dbh "select * from table_a; select * from table b"
              #'print-row)
~~~
More examples:
~~~el
;; Update/delete/insert
(sqlite3-exec dbh "delete from table")

;; Retrieve the metadata of all columns of a table
(sqlite3-exec dbh "pragma table_info(table)" #'print-row)

;; Transaction support
(sqlite3-exec dbh "begin")
....
(sqlite3-exec dbh "commit")
...
(sqlite3-exec dbh "rollback")
~~~
### sqlite3-bind-*
~~~el
(sqlite3-bind-text statement-handle column-no value)
(sqlite3-bind-int64 statement-handle column-no value)
(sqlite3-bind-double statement-handle column-no value)
(sqlite3-bind-null statement-handle column-no)
~~~
The above four functions bind values to a compiled SQL statements.

Please note that column number starts from 1, not 0!
~~~el
(sqlite3-bind-parameter-count statement-handle)
~~~
The above functions returns the number of SQL parameters of a prepared 
statement.
~~~el
(sqlite3-bind-multi statement-handle &rest params)
~~~
`sqlite3-bind-multi` is not part of the official API but is provided for 
convenience.

Example:
~~~el
(sqlite3-bind-multi stmt 1 "a" 1.555 nil) ;; nil for NULL
~~~
### sqlite3-column-*
These column functions retrieve data from the current row of a
result set.
~~~el
(sqlite3-column-count statement-handle)
~~~
Return number of columns in a result set.
~~~e1
(sqlite3-column-type statement-handle column-no)
~~~
Return the type (`sqlite-integer`, `sqlite-float`, `sqlite-text` or
`sqlite-null`) of the specified column. 

Note: Column number starts from 0.
~~~el
(sqlite3-column-text statement-handle column-no)
(sqlite3-column-int64 statement-handle column-no)
(sqlite3-column-double statement-handle column-no)
~~~
The above functions retrieve data of the specified column.

Note: You can call `sqlite3-column-xxx` on a column even 
if `sqlite3-column-type` returns `sqlite-yyy`: the SQLite3 engine will
perform the necessary type conversion.
~~~el
(sqlite3-fetch statement-handle)
~~~
`sqlite3-fetch` is not part of the official API but provided for 
convenience. It returns the current row as a list of values.
## A Note on Garbage Collection
Since Emacs's garbage collection is non-deterministic, it would be 
a good idea 
to manually free database/statement handles once they are not needed.

~~~el
(unwind-protect
  (progn
    (setq dbh (sqlite3-open "..."))
    (setq stmt (sqlite3-prepare dbh "select ..."))
    (.....))
 (sqlite3-finalize stmt)
 (sqlite3-close dbh))
~~~
## Missing features
- BLOB support
- Custom functions written in elisp

## Known Problems
- SQLite3 supports 64 bit integers but Emacs integers are only 61 bits.
For integers > 61 bits you can retrieve them as text as a workaround.
- TEXT fields with embedded NULLs are not supported.
