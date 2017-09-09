;; -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

(require 'sqlite3-api)
(require 'cl)


(ert-deftest sqlite3-test-create-db ()
  (let* ((db-file (make-temp-file "sqlite3"))
	 (dbh (sqlite3-open
	       db-file
	       (logior sqlite-open-create
		       sqlite-open-readwrite)))
	 (stmt))
    (message "Tests:create-db")
    (unwind-protect
	(progn
	  (should (= sqlite-ok (sqlite3-exec dbh "create table temp ( id integer primary key autoincrement )")))
	  (should (= sqlite-ok (sqlite3-exec dbh "insert into temp values (NULL)")))
	  (setq stmt (sqlite3-prepare dbh "select count(*) from temp"))
	  (should (= sqlite-row (sqlite3-step stmt)))
	  (should (= 1 (sqlite3-column-count stmt)))
	  (should (= 1 (sqlite3-column-int64 stmt 0))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh)
      (delete-file db-file))))


(ert-deftest sqlite3-test-memory-db ()
  (let* ((dbh (sqlite3-open
	       ":memory:"
	       (logior sqlite-open-create
		       sqlite-open-readwrite)))
	 (stmt))
    (message "Test:memory-db")
    (unwind-protect
	(progn
	  (should (= sqlite-ok (sqlite3-exec dbh "create table temp ( id integer primary key autoincrement )")))
	  (should (= sqlite-ok (sqlite3-exec dbh "insert into temp values (NULL)")))
	  (setq stmt (sqlite3-prepare dbh "select count(*) from temp"))
	  (should (= sqlite-row (sqlite3-step stmt)))
	  (should (= 1 (sqlite3-column-count stmt)))
	  (should (= 1 (sqlite3-column-int64 stmt 0))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh))))


(ert-deftest sqlite3-test-temp-db ()
  (let* ((dbh (sqlite3-open
	       ""
	       (logior sqlite-open-create
		       sqlite-open-readwrite)))
	 (stmt))
    (message "Test:temp-db")
    (unwind-protect
	(progn
	  (should (= sqlite-ok (sqlite3-exec dbh "create table test ( id integer primary key autoincrement )")))
	  (should (= sqlite-ok (sqlite3-exec dbh "insert into test values (NULL)")))
	  (setq stmt (sqlite3-prepare dbh "select count(*) from test"))
	  (should (= sqlite-row (sqlite3-step stmt)))
	  (should (= 1 (sqlite3-column-count stmt)))
	  (should (= 1 (sqlite3-column-int64 stmt 0))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh))))

(ert-deftest sqlite3-test-bulk-ops ()
  (let ((dbh)
	(stmt))
    (message "Test:bulk-ops")
    (unwind-protect
	(progn
	  (setq dbh (sqlite3-open "" (logior
					   sqlite-open-readwrite
					   sqlite-open-create)))
	  (should (= sqlite-ok (sqlite3-exec dbh "create table test ( id integer primary key autoincrement, rand integer not null )")))
	  (should (setq stmt (sqlite3-prepare dbh "insert into test values (?, ?)")))
	  (should (= 2 (sqlite3-bind-parameter-count stmt)))
	  (sqlite3-exec dbh "begin")
	  (cl-loop for i from 1 to 100000 do
		   (should (= sqlite-ok (sqlite3-bind-null stmt 1)))
		   (should (= sqlite-ok (sqlite3-bind-int64 stmt 2 (random))))
		   (should (= sqlite-done (sqlite3-step stmt)))
		   (should (= sqlite-ok (sqlite3-reset stmt))))
	  (should (= 100000 (sqlite3-last-insert-rowid dbh)))
	  (should (= sqlite-ok (sqlite3-exec dbh "update test set rand = 0")))
	  (should (= 0 (sqlite3-get-autocommit dbh)))
	  (should (= 100000 (sqlite3-changes dbh)))
	  (should (= sqlite-ok (sqlite3-exec dbh "delete from test where id > 5000")))
	  (should (= 95000 (sqlite3-changes dbh)))
	  (sqlite3-exec dbh "commit"))
      (sqlite3-finalize stmt))
      (sqlite3-close dbh)))

(ert-deftest sqlite3-test-datatypes ()
  (let ((dbh)
	(stmt)
	(most-pos-double (ldexp 0.9999999999999999 1024))
	(most-neg-double (ldexp 0.5 -1021))
	(big-str (make-string 10000000 ?1))
	(utf8-str "一二三四五六七"))
    (message "Test:datatypes")
    (unwind-protect
	(progn
	  (setq dbh (sqlite3-open "" (logior sqlite-open-readwrite
					     sqlite-open-create)))
	  (should (= sqlite-ok (sqlite3-exec dbh "create table temp (pos_int integer, neg_int integer, pos_flt real, neg_flt real, big_str text, utf8_str text, more_text text)")))
	  (should (setq stmt (sqlite3-prepare dbh "insert into temp values (?,?,?,?,?,?,?)")))
	  (should (= sqlite-ok (sqlite3-bind-int64 stmt 1 most-positive-fixnum)))
	  (should (= sqlite-ok (sqlite3-bind-int64 stmt 2 most-negative-fixnum)))
	  (should (= sqlite-ok (sqlite3-bind-double stmt 3 most-pos-double)))
	  (should (= sqlite-ok (sqlite3-bind-double stmt 4 most-neg-double)))
	  (should (= sqlite-ok (sqlite3-bind-text stmt 5 big-str)))
	  (should (= sqlite-ok (sqlite3-bind-text stmt 6 utf8-str)))
	  (should (= sqlite-ok (sqlite3-bind-null stmt 7)))
	  (should (= sqlite-done (sqlite3-step stmt)))
	  (should (setq stmt (sqlite3-prepare dbh "select * from temp")))
	  (should (= sqlite-row (sqlite3-step stmt)))
	  (should (= most-positive-fixnum (sqlite3-column-int64 stmt 0)))
	  (should (= most-negative-fixnum (sqlite3-column-int64 stmt 1)))
	  (should (= most-pos-double (sqlite3-column-double stmt 2)))
	  (should (= most-neg-double (sqlite3-column-double stmt 3)))
	  (should (string= big-str (sqlite3-column-text stmt 4)))
	  (should (string= utf8-str (sqlite3-column-text stmt 5)))
	  (should (= sqlite-null (sqlite3-column-type stmt 6))))
      (sqlite3-finalize stmt)
      (sqlite3-close dbh))))

(sqlite3-set-log-level 3)
(ert "^sqlite3-test")
(garbage-collect)


;; update - bind
;; delete - bind
;; delete entire table

;; transaction
;; commit
;; rollback

;; drop table
;; build index
;; drop index
;; add column

;; select

;; pragma
