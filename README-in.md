# SQLite3 API v0.11 for Emacs 25+
`sqlite3-api` is a dynamic module for GNU Emacs 25+ that provides 
direct access to the core SQLite3 C API from Emacs Lisp.
~~~el
(require 'sqlite3-api)

(setq dbh (sqlite3-open "person.sqlite3" sqlite-open-readwrite sqlite-open-create))
(sqlite3-exec dbh "create table temp (name text, age integer)")
(setq stmt (sqlite3-prepare dbh "insert into temp values (?,?)"))
(cl-loop for i from 1 to 10 do
	 (sqlite3-bind-multi stmt (format "name%d" i) i)
	  ;; execute the SQL
	 (sqlite3-step stmt)
	 ;; call reset if you want to bind the SQL to a new set of variables
	 (sqlite3-reset stmt))
(sqlite3-finalize stmt)

(setq stmt (sqlite3-prepare dbh "select * from temp"))
(while (= sqlite-row (sqlite3-step stmt))
  (cl-destructuring-bind (name age) (sqlite3-fetch stmt)
    (message "name: %s, age: %d" name age)))
(sqlite3-finalize stmt)
(sqlite3-close dbh)
~~~
While this module provides only 14 functions (vs [200+ in the C API](https://sqlite.org/c3ref/funclist.html)), it should satisfy most
users' needs.

The current version is v0.11.

This is an alpha release so it might crash your Emacs. Save your work before you try it out!

<<TOC>>
## Requirements
- Emacs 25.1 or above, compiled with module support (`./configure --with-modules`)
- SQLite3 v3.16.0 or above. Older versions might work but I have not personally tested those.
- A C99 compiler

It's been tested on macOS (Sierra) and CentOS 7.
## Installation
~~~sh
$ git co https://github.com/pekingduck/emacs-sqlite3-api
$ cd emacs-sqlite3-api
$ make
$ cp sqlite3-api.so /your/elisp/load-path/
~~~
<!--
~~~sh
$ git co https://github.com/pekingduck/emacs-sqlite3-api
$ cd emacs-sqlite3-api
$ make module
~~~
A tar archive called `sqlite3-api-X.Y.tar` will be created. Do a `M-x package-install-file` in Emacs to install that tar archive and 
you'll all set.
-->
For Mac users:
~~~sh
$ make HOMEBREW=1
~~~
to build the module against sqlite3 installed by homebrew.

If you have sqlite3 installed in a nonstandard location, you can do
~~~sh
$ make INC=/path/to/sqlite3/include LIB="-L/path/to/sqlite3/lib -lsqlite3"
~~~
Currently there's no way to reload a dynamic module in Emacs
(`unload-feature` doesn't seem to work for dynamic modules.)
If you are updating from an older version, you'll need to restart Emacs
for the new module to take effect.

## API
To load the package, put the following in your `.emacs`:
~~~el
(require 'sqlite3-api)
~~~

An application will typically use sqlite3_open() to create a single database connection during initialization. 

To run an SQL statement, the application follows these steps:

1. Create a prepared statement using sqlite3_prepare().
1. Evaluate the prepared statement by calling sqlite3_step() one or more times.
1. For queries, extract results by calling sqlite3_column() in between two calls to sqlite3_step().
1. Destroy the prepared statement using sqlite3_finalize().
1. Close the database using sqlite3_close().

[SQlite3 constants](https://www.sqlite.org/rescode.html), defined in sqlite3.h, are things such as numeric result codes from various interfaces (ex: `SQLITE_OK`) or flags passed into functions to control behavior (ex: `SQLITE_OPEN_READONLY`).

In elisp they are in lowercase and words are separated by "-" instead of
"_". For example, `SQLITE_OK` would be `sqlite-ok`.

[www.sqlite.org](https://www.sqlite.org) is always a good source of information, especially 
[An Introduction to the SQLite C/C++ Interface](https://www.sqlite.org/cintro.html) and [C/C++ API Reference](https://www.sqlite.org/c3ref/intro.html).

### sqlite3-open
~~~el
(sqlite3-open "/path/to/data-file" flag1 flag2 ...)
~~~
Open the database file and return a database handle.

This function calls [`sqlite3_open_v2()`](https://www.sqlite.org/c3ref/open.html) internally and raises `'db-error` in case of error.

*flag1*, *flag2*.... will be ORed together.
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

This function calls [`sqlite3_prepare_v2()`](https://www.sqlite.org/c3ref/prepare.html) internally and raises 'sql-error in case of error.
### sqlite3-finalize
~~~el
(sqlite3-finalize statement-handle1 statement-handle2 ...)
~~~
Destroy prepared statements.
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
the statement to new variables, or to re-execute the prepared statement
from the start.
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
 2. the second parameter is the actual data (as a list strings or nil in case of NULL); 
 3. the third one is a list of column names. 
 
To signal an error condition inside the callback, return `nil`.
`sqlite3_exec()` will stop the execution and raise 'db-error.

Raises db-error in case of error.

An example of a callback:
~~~el
(defun print-row (ncols data names)
  (cl-loop for i from 0 to (1- ncols) do
           (message "[Column %d/%d]%s=%s" (1+ i) ncols (elt names i) (elt data i)))
  (message "--------------------")
  t)
  
(sqlite3-exec dbh "select * from table_a; select * from table b"
              #'print-row)
~~~
More examples:
~~~el
;; Update/delete/insert
(sqlite3-exec dbh "delete from table") ;; delete returns no rows

;; Retrieve the metadata of columns in a table
(sqlite3-exec dbh "pragma table_info(table)" #'print-row)
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
### sqlite3-bind-multi
~~~el
(sqlite3-bind-multi statement-handle &rest params)
~~~
`sqlite3-bind-multi` binds multiple parameters to a prepared SQL 
statement. It is not part of the official API but is provided for 
convenience.

Example:
~~~el
(sqlite3-bind-multi stmt 1234 "a" 1.555 nil) ;; nil for NULL
~~~
### sqlite3-column-*
These column functions are used to retrieve the current row
of the result set.

~~~el
(sqlite3-column-count statement-handle)
~~~
Return number of columns in a result set.
~~~e1
(sqlite3-column-type statement-handle column-no)
~~~
Return the type (`sqlite-integer`, `sqlite-float`, `sqlite3-text` or
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

Example:
~~~el
(setq stmt (sqlite3-prepare dbh "select * from temp"))
(while (= sqlite-row (sqlite3-step stmt))
	(let ((name (sqlite3-column-text stmt 0))
	      (age (sqlite3-column-int64 stmt 1)))
      (message "name: %s, age: %d" name age)))
~~~
### sqlite3-fetch
~~~el
(sqlite3-fetch statement-handle) ;; returns a list such as (123 56 "Peter Smith" nil)
~~~
`sqlite3-fetch` is not part of the official API but provided for 
convenience. It retrieves the current row as a 
list without having to deal with sqlite3-column-* explicitly.

## Transaction Support
Use `sqlite3-exec` to start, commit and rollback a transaction:
~~~el
(sqlite3-exec dbh "begin")
(sqlite3-exec dbh "commit")
(sqlite3-exec dbh "rollback")
~~~
See Error Handling below on how to use the [`condition-case`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html) form to handle rollback.
## Error Handling
Currently two error symbols are defined in `sqlite3-api.el`:
1. `sql-error` is raised by `sqlite3-prepare`
2. `db-error` is raised by `sqlite3-open` and `sqlite3-exec`

~~~el
(condition-case db-err
    (progn
      (sqlite3-exec dbh "begin")
      (sqlite3-exec dbh "update temp set a = 1 where b = 2")
      (sqlite3-exec dbh "commit"))
  (db-error
   (message "Symbol:%s, Message:%s, Error Code:%d" (elt db-err 0) (elt db-err 1) (elt db-err 2))
   (sqlite3-exec dbh "rollback")))
~~~
`db-err` is a list containing the error symbol (`db-error` or `sql-error`), an error message and finally an error code returned from the 
corresponding SQLite
C API.

## A Note on Garbage Collection
Since Emacs's garbage collection is non-deterministic, it would be 
a good idea 
to manually free database/statement handles once they are not needed.

## Known Problems
- SQLite3 supports 64 bit integers but Emacs integers are only 61 bits.
For integers > 61 bits you can retrieve them as text as a workaround.
- BLOB and TEXT columns with embedded NULLs are not supported.

## License
The code is licensed under the [GNU GPL v3](https://www.gnu.org/licenses/gpl-3.0.html).

## Changelog
*v0.11 - 2017-09-14*
- `sqlite3-finalize` now accepts multiple handles.

*v0.1 - 2017-09-04*
- Emacs Lisp code removed. The package is now pure C.

*v0.0 - 2017-08-29*
- Fixed a memory leak in `sql_api_exec()`
- Changed `sqlite3_close()` to `sqlite3_close_v2()` in `sqlite_api_close()`
- Better error handling: Error code is returned along with error message
## Useful Links for Writing Dynamic Modules
- https://phst.github.io/emacs-modules
- http://nullprogram.com/blog/2016/11/05/
