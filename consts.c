/*
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
  Auto-generated 2020-07-10 12:21:25
  Based on https://sqlite.org/c3ref/constlist.html
*/

#ifdef SQLITE_VERSION
defconst(env, "sqlite-version", env->make_string(env, SQLITE_VERSION, strlen(SQLITE_VERSION)));
#endif
#ifdef SQLITE_VERSION_NUMBER
defconst(env, "sqlite-version-number", env->make_integer(env, SQLITE_VERSION_NUMBER));
#endif
#ifdef SQLITE_SOURCE_ID
defconst(env, "sqlite-source-id", env->make_string(env, SQLITE_SOURCE_ID, strlen(SQLITE_SOURCE_ID)));
#endif
#ifdef SQLITE_OK
defconst(env, "sqlite-ok", env->make_integer(env, SQLITE_OK));
#endif
#ifdef SQLITE_ERROR
defconst(env, "sqlite-error", env->make_integer(env, SQLITE_ERROR));
#endif
#ifdef SQLITE_INTERNAL
defconst(env, "sqlite-internal", env->make_integer(env, SQLITE_INTERNAL));
#endif
#ifdef SQLITE_PERM
defconst(env, "sqlite-perm", env->make_integer(env, SQLITE_PERM));
#endif
#ifdef SQLITE_ABORT
defconst(env, "sqlite-abort", env->make_integer(env, SQLITE_ABORT));
#endif
#ifdef SQLITE_BUSY
defconst(env, "sqlite-busy", env->make_integer(env, SQLITE_BUSY));
#endif
#ifdef SQLITE_LOCKED
defconst(env, "sqlite-locked", env->make_integer(env, SQLITE_LOCKED));
#endif
#ifdef SQLITE_NOMEM
defconst(env, "sqlite-nomem", env->make_integer(env, SQLITE_NOMEM));
#endif
#ifdef SQLITE_READONLY
defconst(env, "sqlite-readonly", env->make_integer(env, SQLITE_READONLY));
#endif
#ifdef SQLITE_INTERRUPT
defconst(env, "sqlite-interrupt", env->make_integer(env, SQLITE_INTERRUPT));
#endif
#ifdef SQLITE_IOERR
defconst(env, "sqlite-ioerr", env->make_integer(env, SQLITE_IOERR));
#endif
#ifdef SQLITE_CORRUPT
defconst(env, "sqlite-corrupt", env->make_integer(env, SQLITE_CORRUPT));
#endif
#ifdef SQLITE_NOTFOUND
defconst(env, "sqlite-notfound", env->make_integer(env, SQLITE_NOTFOUND));
#endif
#ifdef SQLITE_FULL
defconst(env, "sqlite-full", env->make_integer(env, SQLITE_FULL));
#endif
#ifdef SQLITE_CANTOPEN
defconst(env, "sqlite-cantopen", env->make_integer(env, SQLITE_CANTOPEN));
#endif
#ifdef SQLITE_PROTOCOL
defconst(env, "sqlite-protocol", env->make_integer(env, SQLITE_PROTOCOL));
#endif
#ifdef SQLITE_EMPTY
defconst(env, "sqlite-empty", env->make_integer(env, SQLITE_EMPTY));
#endif
#ifdef SQLITE_SCHEMA
defconst(env, "sqlite-schema", env->make_integer(env, SQLITE_SCHEMA));
#endif
#ifdef SQLITE_TOOBIG
defconst(env, "sqlite-toobig", env->make_integer(env, SQLITE_TOOBIG));
#endif
#ifdef SQLITE_CONSTRAINT
defconst(env, "sqlite-constraint", env->make_integer(env, SQLITE_CONSTRAINT));
#endif
#ifdef SQLITE_MISMATCH
defconst(env, "sqlite-mismatch", env->make_integer(env, SQLITE_MISMATCH));
#endif
#ifdef SQLITE_MISUSE
defconst(env, "sqlite-misuse", env->make_integer(env, SQLITE_MISUSE));
#endif
#ifdef SQLITE_NOLFS
defconst(env, "sqlite-nolfs", env->make_integer(env, SQLITE_NOLFS));
#endif
#ifdef SQLITE_AUTH
defconst(env, "sqlite-auth", env->make_integer(env, SQLITE_AUTH));
#endif
#ifdef SQLITE_FORMAT
defconst(env, "sqlite-format", env->make_integer(env, SQLITE_FORMAT));
#endif
#ifdef SQLITE_RANGE
defconst(env, "sqlite-range", env->make_integer(env, SQLITE_RANGE));
#endif
#ifdef SQLITE_NOTADB
defconst(env, "sqlite-notadb", env->make_integer(env, SQLITE_NOTADB));
#endif
#ifdef SQLITE_NOTICE
defconst(env, "sqlite-notice", env->make_integer(env, SQLITE_NOTICE));
#endif
#ifdef SQLITE_WARNING
defconst(env, "sqlite-warning", env->make_integer(env, SQLITE_WARNING));
#endif
#ifdef SQLITE_ROW
defconst(env, "sqlite-row", env->make_integer(env, SQLITE_ROW));
#endif
#ifdef SQLITE_DONE
defconst(env, "sqlite-done", env->make_integer(env, SQLITE_DONE));
#endif
#ifdef SQLITE_ERROR_MISSING_COLLSEQ
defconst(env, "sqlite-error-missing-collseq", env->make_integer(env, SQLITE_ERROR_MISSING_COLLSEQ));
#endif
#ifdef SQLITE_ERROR_RETRY
defconst(env, "sqlite-error-retry", env->make_integer(env, SQLITE_ERROR_RETRY));
#endif
#ifdef SQLITE_ERROR_SNAPSHOT
defconst(env, "sqlite-error-snapshot", env->make_integer(env, SQLITE_ERROR_SNAPSHOT));
#endif
#ifdef SQLITE_IOERR_READ
defconst(env, "sqlite-ioerr-read", env->make_integer(env, SQLITE_IOERR_READ));
#endif
#ifdef SQLITE_IOERR_SHORT_READ
defconst(env, "sqlite-ioerr-short-read", env->make_integer(env, SQLITE_IOERR_SHORT_READ));
#endif
#ifdef SQLITE_IOERR_WRITE
defconst(env, "sqlite-ioerr-write", env->make_integer(env, SQLITE_IOERR_WRITE));
#endif
#ifdef SQLITE_IOERR_FSYNC
defconst(env, "sqlite-ioerr-fsync", env->make_integer(env, SQLITE_IOERR_FSYNC));
#endif
#ifdef SQLITE_IOERR_DIR_FSYNC
defconst(env, "sqlite-ioerr-dir-fsync", env->make_integer(env, SQLITE_IOERR_DIR_FSYNC));
#endif
#ifdef SQLITE_IOERR_TRUNCATE
defconst(env, "sqlite-ioerr-truncate", env->make_integer(env, SQLITE_IOERR_TRUNCATE));
#endif
#ifdef SQLITE_IOERR_FSTAT
defconst(env, "sqlite-ioerr-fstat", env->make_integer(env, SQLITE_IOERR_FSTAT));
#endif
#ifdef SQLITE_IOERR_UNLOCK
defconst(env, "sqlite-ioerr-unlock", env->make_integer(env, SQLITE_IOERR_UNLOCK));
#endif
#ifdef SQLITE_IOERR_RDLOCK
defconst(env, "sqlite-ioerr-rdlock", env->make_integer(env, SQLITE_IOERR_RDLOCK));
#endif
#ifdef SQLITE_IOERR_DELETE
defconst(env, "sqlite-ioerr-delete", env->make_integer(env, SQLITE_IOERR_DELETE));
#endif
#ifdef SQLITE_IOERR_BLOCKED
defconst(env, "sqlite-ioerr-blocked", env->make_integer(env, SQLITE_IOERR_BLOCKED));
#endif
#ifdef SQLITE_IOERR_NOMEM
defconst(env, "sqlite-ioerr-nomem", env->make_integer(env, SQLITE_IOERR_NOMEM));
#endif
#ifdef SQLITE_IOERR_ACCESS
defconst(env, "sqlite-ioerr-access", env->make_integer(env, SQLITE_IOERR_ACCESS));
#endif
#ifdef SQLITE_IOERR_CHECKRESERVEDLOCK
defconst(env, "sqlite-ioerr-checkreservedlock", env->make_integer(env, SQLITE_IOERR_CHECKRESERVEDLOCK));
#endif
#ifdef SQLITE_IOERR_LOCK
defconst(env, "sqlite-ioerr-lock", env->make_integer(env, SQLITE_IOERR_LOCK));
#endif
#ifdef SQLITE_IOERR_CLOSE
defconst(env, "sqlite-ioerr-close", env->make_integer(env, SQLITE_IOERR_CLOSE));
#endif
#ifdef SQLITE_IOERR_DIR_CLOSE
defconst(env, "sqlite-ioerr-dir-close", env->make_integer(env, SQLITE_IOERR_DIR_CLOSE));
#endif
#ifdef SQLITE_IOERR_SHMOPEN
defconst(env, "sqlite-ioerr-shmopen", env->make_integer(env, SQLITE_IOERR_SHMOPEN));
#endif
#ifdef SQLITE_IOERR_SHMSIZE
defconst(env, "sqlite-ioerr-shmsize", env->make_integer(env, SQLITE_IOERR_SHMSIZE));
#endif
#ifdef SQLITE_IOERR_SHMLOCK
defconst(env, "sqlite-ioerr-shmlock", env->make_integer(env, SQLITE_IOERR_SHMLOCK));
#endif
#ifdef SQLITE_IOERR_SHMMAP
defconst(env, "sqlite-ioerr-shmmap", env->make_integer(env, SQLITE_IOERR_SHMMAP));
#endif
#ifdef SQLITE_IOERR_SEEK
defconst(env, "sqlite-ioerr-seek", env->make_integer(env, SQLITE_IOERR_SEEK));
#endif
#ifdef SQLITE_IOERR_DELETE_NOENT
defconst(env, "sqlite-ioerr-delete-noent", env->make_integer(env, SQLITE_IOERR_DELETE_NOENT));
#endif
#ifdef SQLITE_IOERR_MMAP
defconst(env, "sqlite-ioerr-mmap", env->make_integer(env, SQLITE_IOERR_MMAP));
#endif
#ifdef SQLITE_IOERR_GETTEMPPATH
defconst(env, "sqlite-ioerr-gettemppath", env->make_integer(env, SQLITE_IOERR_GETTEMPPATH));
#endif
#ifdef SQLITE_IOERR_CONVPATH
defconst(env, "sqlite-ioerr-convpath", env->make_integer(env, SQLITE_IOERR_CONVPATH));
#endif
#ifdef SQLITE_IOERR_VNODE
defconst(env, "sqlite-ioerr-vnode", env->make_integer(env, SQLITE_IOERR_VNODE));
#endif
#ifdef SQLITE_IOERR_AUTH
defconst(env, "sqlite-ioerr-auth", env->make_integer(env, SQLITE_IOERR_AUTH));
#endif
#ifdef SQLITE_IOERR_BEGIN_ATOMIC
defconst(env, "sqlite-ioerr-begin-atomic", env->make_integer(env, SQLITE_IOERR_BEGIN_ATOMIC));
#endif
#ifdef SQLITE_IOERR_COMMIT_ATOMIC
defconst(env, "sqlite-ioerr-commit-atomic", env->make_integer(env, SQLITE_IOERR_COMMIT_ATOMIC));
#endif
#ifdef SQLITE_IOERR_ROLLBACK_ATOMIC
defconst(env, "sqlite-ioerr-rollback-atomic", env->make_integer(env, SQLITE_IOERR_ROLLBACK_ATOMIC));
#endif
#ifdef SQLITE_IOERR_DATA
defconst(env, "sqlite-ioerr-data", env->make_integer(env, SQLITE_IOERR_DATA));
#endif
#ifdef SQLITE_LOCKED_SHAREDCACHE
defconst(env, "sqlite-locked-sharedcache", env->make_integer(env, SQLITE_LOCKED_SHAREDCACHE));
#endif
#ifdef SQLITE_LOCKED_VTAB
defconst(env, "sqlite-locked-vtab", env->make_integer(env, SQLITE_LOCKED_VTAB));
#endif
#ifdef SQLITE_BUSY_RECOVERY
defconst(env, "sqlite-busy-recovery", env->make_integer(env, SQLITE_BUSY_RECOVERY));
#endif
#ifdef SQLITE_BUSY_SNAPSHOT
defconst(env, "sqlite-busy-snapshot", env->make_integer(env, SQLITE_BUSY_SNAPSHOT));
#endif
#ifdef SQLITE_BUSY_TIMEOUT
defconst(env, "sqlite-busy-timeout", env->make_integer(env, SQLITE_BUSY_TIMEOUT));
#endif
#ifdef SQLITE_CANTOPEN_NOTEMPDIR
defconst(env, "sqlite-cantopen-notempdir", env->make_integer(env, SQLITE_CANTOPEN_NOTEMPDIR));
#endif
#ifdef SQLITE_CANTOPEN_ISDIR
defconst(env, "sqlite-cantopen-isdir", env->make_integer(env, SQLITE_CANTOPEN_ISDIR));
#endif
#ifdef SQLITE_CANTOPEN_FULLPATH
defconst(env, "sqlite-cantopen-fullpath", env->make_integer(env, SQLITE_CANTOPEN_FULLPATH));
#endif
#ifdef SQLITE_CANTOPEN_CONVPATH
defconst(env, "sqlite-cantopen-convpath", env->make_integer(env, SQLITE_CANTOPEN_CONVPATH));
#endif
#ifdef SQLITE_CANTOPEN_DIRTYWAL
defconst(env, "sqlite-cantopen-dirtywal", env->make_integer(env, SQLITE_CANTOPEN_DIRTYWAL));
#endif
#ifdef SQLITE_CANTOPEN_SYMLINK
defconst(env, "sqlite-cantopen-symlink", env->make_integer(env, SQLITE_CANTOPEN_SYMLINK));
#endif
#ifdef SQLITE_CORRUPT_VTAB
defconst(env, "sqlite-corrupt-vtab", env->make_integer(env, SQLITE_CORRUPT_VTAB));
#endif
#ifdef SQLITE_CORRUPT_SEQUENCE
defconst(env, "sqlite-corrupt-sequence", env->make_integer(env, SQLITE_CORRUPT_SEQUENCE));
#endif
#ifdef SQLITE_CORRUPT_INDEX
defconst(env, "sqlite-corrupt-index", env->make_integer(env, SQLITE_CORRUPT_INDEX));
#endif
#ifdef SQLITE_READONLY_RECOVERY
defconst(env, "sqlite-readonly-recovery", env->make_integer(env, SQLITE_READONLY_RECOVERY));
#endif
#ifdef SQLITE_READONLY_CANTLOCK
defconst(env, "sqlite-readonly-cantlock", env->make_integer(env, SQLITE_READONLY_CANTLOCK));
#endif
#ifdef SQLITE_READONLY_ROLLBACK
defconst(env, "sqlite-readonly-rollback", env->make_integer(env, SQLITE_READONLY_ROLLBACK));
#endif
#ifdef SQLITE_READONLY_DBMOVED
defconst(env, "sqlite-readonly-dbmoved", env->make_integer(env, SQLITE_READONLY_DBMOVED));
#endif
#ifdef SQLITE_READONLY_CANTINIT
defconst(env, "sqlite-readonly-cantinit", env->make_integer(env, SQLITE_READONLY_CANTINIT));
#endif
#ifdef SQLITE_READONLY_DIRECTORY
defconst(env, "sqlite-readonly-directory", env->make_integer(env, SQLITE_READONLY_DIRECTORY));
#endif
#ifdef SQLITE_ABORT_ROLLBACK
defconst(env, "sqlite-abort-rollback", env->make_integer(env, SQLITE_ABORT_ROLLBACK));
#endif
#ifdef SQLITE_CONSTRAINT_CHECK
defconst(env, "sqlite-constraint-check", env->make_integer(env, SQLITE_CONSTRAINT_CHECK));
#endif
#ifdef SQLITE_CONSTRAINT_COMMITHOOK
defconst(env, "sqlite-constraint-commithook", env->make_integer(env, SQLITE_CONSTRAINT_COMMITHOOK));
#endif
#ifdef SQLITE_CONSTRAINT_FOREIGNKEY
defconst(env, "sqlite-constraint-foreignkey", env->make_integer(env, SQLITE_CONSTRAINT_FOREIGNKEY));
#endif
#ifdef SQLITE_CONSTRAINT_FUNCTION
defconst(env, "sqlite-constraint-function", env->make_integer(env, SQLITE_CONSTRAINT_FUNCTION));
#endif
#ifdef SQLITE_CONSTRAINT_NOTNULL
defconst(env, "sqlite-constraint-notnull", env->make_integer(env, SQLITE_CONSTRAINT_NOTNULL));
#endif
#ifdef SQLITE_CONSTRAINT_PRIMARYKEY
defconst(env, "sqlite-constraint-primarykey", env->make_integer(env, SQLITE_CONSTRAINT_PRIMARYKEY));
#endif
#ifdef SQLITE_CONSTRAINT_TRIGGER
defconst(env, "sqlite-constraint-trigger", env->make_integer(env, SQLITE_CONSTRAINT_TRIGGER));
#endif
#ifdef SQLITE_CONSTRAINT_UNIQUE
defconst(env, "sqlite-constraint-unique", env->make_integer(env, SQLITE_CONSTRAINT_UNIQUE));
#endif
#ifdef SQLITE_CONSTRAINT_VTAB
defconst(env, "sqlite-constraint-vtab", env->make_integer(env, SQLITE_CONSTRAINT_VTAB));
#endif
#ifdef SQLITE_CONSTRAINT_ROWID
defconst(env, "sqlite-constraint-rowid", env->make_integer(env, SQLITE_CONSTRAINT_ROWID));
#endif
#ifdef SQLITE_CONSTRAINT_PINNED
defconst(env, "sqlite-constraint-pinned", env->make_integer(env, SQLITE_CONSTRAINT_PINNED));
#endif
#ifdef SQLITE_NOTICE_RECOVER_WAL
defconst(env, "sqlite-notice-recover-wal", env->make_integer(env, SQLITE_NOTICE_RECOVER_WAL));
#endif
#ifdef SQLITE_NOTICE_RECOVER_ROLLBACK
defconst(env, "sqlite-notice-recover-rollback", env->make_integer(env, SQLITE_NOTICE_RECOVER_ROLLBACK));
#endif
#ifdef SQLITE_WARNING_AUTOINDEX
defconst(env, "sqlite-warning-autoindex", env->make_integer(env, SQLITE_WARNING_AUTOINDEX));
#endif
#ifdef SQLITE_AUTH_USER
defconst(env, "sqlite-auth-user", env->make_integer(env, SQLITE_AUTH_USER));
#endif
#ifdef SQLITE_OK_LOAD_PERMANENTLY
defconst(env, "sqlite-ok-load-permanently", env->make_integer(env, SQLITE_OK_LOAD_PERMANENTLY));
#endif
#ifdef SQLITE_OK_SYMLINK
defconst(env, "sqlite-ok-symlink", env->make_integer(env, SQLITE_OK_SYMLINK));
#endif
#ifdef SQLITE_OPEN_READONLY
defconst(env, "sqlite-open-readonly", env->make_integer(env, SQLITE_OPEN_READONLY));
#endif
#ifdef SQLITE_OPEN_READWRITE
defconst(env, "sqlite-open-readwrite", env->make_integer(env, SQLITE_OPEN_READWRITE));
#endif
#ifdef SQLITE_OPEN_CREATE
defconst(env, "sqlite-open-create", env->make_integer(env, SQLITE_OPEN_CREATE));
#endif
#ifdef SQLITE_OPEN_DELETEONCLOSE
defconst(env, "sqlite-open-deleteonclose", env->make_integer(env, SQLITE_OPEN_DELETEONCLOSE));
#endif
#ifdef SQLITE_OPEN_EXCLUSIVE
defconst(env, "sqlite-open-exclusive", env->make_integer(env, SQLITE_OPEN_EXCLUSIVE));
#endif
#ifdef SQLITE_OPEN_AUTOPROXY
defconst(env, "sqlite-open-autoproxy", env->make_integer(env, SQLITE_OPEN_AUTOPROXY));
#endif
#ifdef SQLITE_OPEN_URI
defconst(env, "sqlite-open-uri", env->make_integer(env, SQLITE_OPEN_URI));
#endif
#ifdef SQLITE_OPEN_MEMORY
defconst(env, "sqlite-open-memory", env->make_integer(env, SQLITE_OPEN_MEMORY));
#endif
#ifdef SQLITE_OPEN_MAIN_DB
defconst(env, "sqlite-open-main-db", env->make_integer(env, SQLITE_OPEN_MAIN_DB));
#endif
#ifdef SQLITE_OPEN_TEMP_DB
defconst(env, "sqlite-open-temp-db", env->make_integer(env, SQLITE_OPEN_TEMP_DB));
#endif
#ifdef SQLITE_OPEN_TRANSIENT_DB
defconst(env, "sqlite-open-transient-db", env->make_integer(env, SQLITE_OPEN_TRANSIENT_DB));
#endif
#ifdef SQLITE_OPEN_MAIN_JOURNAL
defconst(env, "sqlite-open-main-journal", env->make_integer(env, SQLITE_OPEN_MAIN_JOURNAL));
#endif
#ifdef SQLITE_OPEN_TEMP_JOURNAL
defconst(env, "sqlite-open-temp-journal", env->make_integer(env, SQLITE_OPEN_TEMP_JOURNAL));
#endif
#ifdef SQLITE_OPEN_SUBJOURNAL
defconst(env, "sqlite-open-subjournal", env->make_integer(env, SQLITE_OPEN_SUBJOURNAL));
#endif
#ifdef SQLITE_OPEN_MASTER_JOURNAL
defconst(env, "sqlite-open-master-journal", env->make_integer(env, SQLITE_OPEN_MASTER_JOURNAL));
#endif
#ifdef SQLITE_OPEN_NOMUTEX
defconst(env, "sqlite-open-nomutex", env->make_integer(env, SQLITE_OPEN_NOMUTEX));
#endif
#ifdef SQLITE_OPEN_FULLMUTEX
defconst(env, "sqlite-open-fullmutex", env->make_integer(env, SQLITE_OPEN_FULLMUTEX));
#endif
#ifdef SQLITE_OPEN_SHAREDCACHE
defconst(env, "sqlite-open-sharedcache", env->make_integer(env, SQLITE_OPEN_SHAREDCACHE));
#endif
#ifdef SQLITE_OPEN_PRIVATECACHE
defconst(env, "sqlite-open-privatecache", env->make_integer(env, SQLITE_OPEN_PRIVATECACHE));
#endif
#ifdef SQLITE_OPEN_WAL
defconst(env, "sqlite-open-wal", env->make_integer(env, SQLITE_OPEN_WAL));
#endif
#ifdef SQLITE_OPEN_NOFOLLOW
defconst(env, "sqlite-open-nofollow", env->make_integer(env, SQLITE_OPEN_NOFOLLOW));
#endif
#ifdef SQLITE_IOCAP_ATOMIC
defconst(env, "sqlite-iocap-atomic", env->make_integer(env, SQLITE_IOCAP_ATOMIC));
#endif
#ifdef SQLITE_IOCAP_ATOMIC512
defconst(env, "sqlite-iocap-atomic512", env->make_integer(env, SQLITE_IOCAP_ATOMIC512));
#endif
#ifdef SQLITE_IOCAP_ATOMIC1K
defconst(env, "sqlite-iocap-atomic1k", env->make_integer(env, SQLITE_IOCAP_ATOMIC1K));
#endif
#ifdef SQLITE_IOCAP_ATOMIC2K
defconst(env, "sqlite-iocap-atomic2k", env->make_integer(env, SQLITE_IOCAP_ATOMIC2K));
#endif
#ifdef SQLITE_IOCAP_ATOMIC4K
defconst(env, "sqlite-iocap-atomic4k", env->make_integer(env, SQLITE_IOCAP_ATOMIC4K));
#endif
#ifdef SQLITE_IOCAP_ATOMIC8K
defconst(env, "sqlite-iocap-atomic8k", env->make_integer(env, SQLITE_IOCAP_ATOMIC8K));
#endif
#ifdef SQLITE_IOCAP_ATOMIC16K
defconst(env, "sqlite-iocap-atomic16k", env->make_integer(env, SQLITE_IOCAP_ATOMIC16K));
#endif
#ifdef SQLITE_IOCAP_ATOMIC32K
defconst(env, "sqlite-iocap-atomic32k", env->make_integer(env, SQLITE_IOCAP_ATOMIC32K));
#endif
#ifdef SQLITE_IOCAP_ATOMIC64K
defconst(env, "sqlite-iocap-atomic64k", env->make_integer(env, SQLITE_IOCAP_ATOMIC64K));
#endif
#ifdef SQLITE_IOCAP_SAFE_APPEND
defconst(env, "sqlite-iocap-safe-append", env->make_integer(env, SQLITE_IOCAP_SAFE_APPEND));
#endif
#ifdef SQLITE_IOCAP_SEQUENTIAL
defconst(env, "sqlite-iocap-sequential", env->make_integer(env, SQLITE_IOCAP_SEQUENTIAL));
#endif
#ifdef SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN
defconst(env, "sqlite-iocap-undeletable-when-open", env->make_integer(env, SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN));
#endif
#ifdef SQLITE_IOCAP_POWERSAFE_OVERWRITE
defconst(env, "sqlite-iocap-powersafe-overwrite", env->make_integer(env, SQLITE_IOCAP_POWERSAFE_OVERWRITE));
#endif
#ifdef SQLITE_IOCAP_IMMUTABLE
defconst(env, "sqlite-iocap-immutable", env->make_integer(env, SQLITE_IOCAP_IMMUTABLE));
#endif
#ifdef SQLITE_IOCAP_BATCH_ATOMIC
defconst(env, "sqlite-iocap-batch-atomic", env->make_integer(env, SQLITE_IOCAP_BATCH_ATOMIC));
#endif
#ifdef SQLITE_LOCK_NONE
defconst(env, "sqlite-lock-none", env->make_integer(env, SQLITE_LOCK_NONE));
#endif
#ifdef SQLITE_LOCK_SHARED
defconst(env, "sqlite-lock-shared", env->make_integer(env, SQLITE_LOCK_SHARED));
#endif
#ifdef SQLITE_LOCK_RESERVED
defconst(env, "sqlite-lock-reserved", env->make_integer(env, SQLITE_LOCK_RESERVED));
#endif
#ifdef SQLITE_LOCK_PENDING
defconst(env, "sqlite-lock-pending", env->make_integer(env, SQLITE_LOCK_PENDING));
#endif
#ifdef SQLITE_LOCK_EXCLUSIVE
defconst(env, "sqlite-lock-exclusive", env->make_integer(env, SQLITE_LOCK_EXCLUSIVE));
#endif
#ifdef SQLITE_SYNC_NORMAL
defconst(env, "sqlite-sync-normal", env->make_integer(env, SQLITE_SYNC_NORMAL));
#endif
#ifdef SQLITE_SYNC_FULL
defconst(env, "sqlite-sync-full", env->make_integer(env, SQLITE_SYNC_FULL));
#endif
#ifdef SQLITE_SYNC_DATAONLY
defconst(env, "sqlite-sync-dataonly", env->make_integer(env, SQLITE_SYNC_DATAONLY));
#endif
#ifdef SQLITE_FCNTL_LOCKSTATE
defconst(env, "sqlite-fcntl-lockstate", env->make_integer(env, SQLITE_FCNTL_LOCKSTATE));
#endif
#ifdef SQLITE_FCNTL_GET_LOCKPROXYFILE
defconst(env, "sqlite-fcntl-get-lockproxyfile", env->make_integer(env, SQLITE_FCNTL_GET_LOCKPROXYFILE));
#endif
#ifdef SQLITE_FCNTL_SET_LOCKPROXYFILE
defconst(env, "sqlite-fcntl-set-lockproxyfile", env->make_integer(env, SQLITE_FCNTL_SET_LOCKPROXYFILE));
#endif
#ifdef SQLITE_FCNTL_LAST_ERRNO
defconst(env, "sqlite-fcntl-last-errno", env->make_integer(env, SQLITE_FCNTL_LAST_ERRNO));
#endif
#ifdef SQLITE_FCNTL_SIZE_HINT
defconst(env, "sqlite-fcntl-size-hint", env->make_integer(env, SQLITE_FCNTL_SIZE_HINT));
#endif
#ifdef SQLITE_FCNTL_CHUNK_SIZE
defconst(env, "sqlite-fcntl-chunk-size", env->make_integer(env, SQLITE_FCNTL_CHUNK_SIZE));
#endif
#ifdef SQLITE_FCNTL_FILE_POINTER
defconst(env, "sqlite-fcntl-file-pointer", env->make_integer(env, SQLITE_FCNTL_FILE_POINTER));
#endif
#ifdef SQLITE_FCNTL_SYNC_OMITTED
defconst(env, "sqlite-fcntl-sync-omitted", env->make_integer(env, SQLITE_FCNTL_SYNC_OMITTED));
#endif
#ifdef SQLITE_FCNTL_WIN32_AV_RETRY
defconst(env, "sqlite-fcntl-win32-av-retry", env->make_integer(env, SQLITE_FCNTL_WIN32_AV_RETRY));
#endif
#ifdef SQLITE_FCNTL_PERSIST_WAL
defconst(env, "sqlite-fcntl-persist-wal", env->make_integer(env, SQLITE_FCNTL_PERSIST_WAL));
#endif
#ifdef SQLITE_FCNTL_OVERWRITE
defconst(env, "sqlite-fcntl-overwrite", env->make_integer(env, SQLITE_FCNTL_OVERWRITE));
#endif
#ifdef SQLITE_FCNTL_VFSNAME
defconst(env, "sqlite-fcntl-vfsname", env->make_integer(env, SQLITE_FCNTL_VFSNAME));
#endif
#ifdef SQLITE_FCNTL_POWERSAFE_OVERWRITE
defconst(env, "sqlite-fcntl-powersafe-overwrite", env->make_integer(env, SQLITE_FCNTL_POWERSAFE_OVERWRITE));
#endif
#ifdef SQLITE_FCNTL_PRAGMA
defconst(env, "sqlite-fcntl-pragma", env->make_integer(env, SQLITE_FCNTL_PRAGMA));
#endif
#ifdef SQLITE_FCNTL_BUSYHANDLER
defconst(env, "sqlite-fcntl-busyhandler", env->make_integer(env, SQLITE_FCNTL_BUSYHANDLER));
#endif
#ifdef SQLITE_FCNTL_TEMPFILENAME
defconst(env, "sqlite-fcntl-tempfilename", env->make_integer(env, SQLITE_FCNTL_TEMPFILENAME));
#endif
#ifdef SQLITE_FCNTL_MMAP_SIZE
defconst(env, "sqlite-fcntl-mmap-size", env->make_integer(env, SQLITE_FCNTL_MMAP_SIZE));
#endif
#ifdef SQLITE_FCNTL_TRACE
defconst(env, "sqlite-fcntl-trace", env->make_integer(env, SQLITE_FCNTL_TRACE));
#endif
#ifdef SQLITE_FCNTL_HAS_MOVED
defconst(env, "sqlite-fcntl-has-moved", env->make_integer(env, SQLITE_FCNTL_HAS_MOVED));
#endif
#ifdef SQLITE_FCNTL_SYNC
defconst(env, "sqlite-fcntl-sync", env->make_integer(env, SQLITE_FCNTL_SYNC));
#endif
#ifdef SQLITE_FCNTL_COMMIT_PHASETWO
defconst(env, "sqlite-fcntl-commit-phasetwo", env->make_integer(env, SQLITE_FCNTL_COMMIT_PHASETWO));
#endif
#ifdef SQLITE_FCNTL_WIN32_SET_HANDLE
defconst(env, "sqlite-fcntl-win32-set-handle", env->make_integer(env, SQLITE_FCNTL_WIN32_SET_HANDLE));
#endif
#ifdef SQLITE_FCNTL_WAL_BLOCK
defconst(env, "sqlite-fcntl-wal-block", env->make_integer(env, SQLITE_FCNTL_WAL_BLOCK));
#endif
#ifdef SQLITE_FCNTL_ZIPVFS
defconst(env, "sqlite-fcntl-zipvfs", env->make_integer(env, SQLITE_FCNTL_ZIPVFS));
#endif
#ifdef SQLITE_FCNTL_RBU
defconst(env, "sqlite-fcntl-rbu", env->make_integer(env, SQLITE_FCNTL_RBU));
#endif
#ifdef SQLITE_FCNTL_VFS_POINTER
defconst(env, "sqlite-fcntl-vfs-pointer", env->make_integer(env, SQLITE_FCNTL_VFS_POINTER));
#endif
#ifdef SQLITE_FCNTL_JOURNAL_POINTER
defconst(env, "sqlite-fcntl-journal-pointer", env->make_integer(env, SQLITE_FCNTL_JOURNAL_POINTER));
#endif
#ifdef SQLITE_FCNTL_WIN32_GET_HANDLE
defconst(env, "sqlite-fcntl-win32-get-handle", env->make_integer(env, SQLITE_FCNTL_WIN32_GET_HANDLE));
#endif
#ifdef SQLITE_FCNTL_PDB
defconst(env, "sqlite-fcntl-pdb", env->make_integer(env, SQLITE_FCNTL_PDB));
#endif
#ifdef SQLITE_FCNTL_BEGIN_ATOMIC_WRITE
defconst(env, "sqlite-fcntl-begin-atomic-write", env->make_integer(env, SQLITE_FCNTL_BEGIN_ATOMIC_WRITE));
#endif
#ifdef SQLITE_FCNTL_COMMIT_ATOMIC_WRITE
defconst(env, "sqlite-fcntl-commit-atomic-write", env->make_integer(env, SQLITE_FCNTL_COMMIT_ATOMIC_WRITE));
#endif
#ifdef SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE
defconst(env, "sqlite-fcntl-rollback-atomic-write", env->make_integer(env, SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE));
#endif
#ifdef SQLITE_FCNTL_LOCK_TIMEOUT
defconst(env, "sqlite-fcntl-lock-timeout", env->make_integer(env, SQLITE_FCNTL_LOCK_TIMEOUT));
#endif
#ifdef SQLITE_FCNTL_DATA_VERSION
defconst(env, "sqlite-fcntl-data-version", env->make_integer(env, SQLITE_FCNTL_DATA_VERSION));
#endif
#ifdef SQLITE_FCNTL_SIZE_LIMIT
defconst(env, "sqlite-fcntl-size-limit", env->make_integer(env, SQLITE_FCNTL_SIZE_LIMIT));
#endif
#ifdef SQLITE_FCNTL_CKPT_DONE
defconst(env, "sqlite-fcntl-ckpt-done", env->make_integer(env, SQLITE_FCNTL_CKPT_DONE));
#endif
#ifdef SQLITE_FCNTL_RESERVE_BYTES
defconst(env, "sqlite-fcntl-reserve-bytes", env->make_integer(env, SQLITE_FCNTL_RESERVE_BYTES));
#endif
#ifdef SQLITE_FCNTL_CKPT_START
defconst(env, "sqlite-fcntl-ckpt-start", env->make_integer(env, SQLITE_FCNTL_CKPT_START));
#endif
#ifdef SQLITE_ACCESS_EXISTS
defconst(env, "sqlite-access-exists", env->make_integer(env, SQLITE_ACCESS_EXISTS));
#endif
#ifdef SQLITE_ACCESS_READWRITE
defconst(env, "sqlite-access-readwrite", env->make_integer(env, SQLITE_ACCESS_READWRITE));
#endif
#ifdef SQLITE_ACCESS_READ
defconst(env, "sqlite-access-read", env->make_integer(env, SQLITE_ACCESS_READ));
#endif
#ifdef SQLITE_SHM_UNLOCK
defconst(env, "sqlite-shm-unlock", env->make_integer(env, SQLITE_SHM_UNLOCK));
#endif
#ifdef SQLITE_SHM_LOCK
defconst(env, "sqlite-shm-lock", env->make_integer(env, SQLITE_SHM_LOCK));
#endif
#ifdef SQLITE_SHM_SHARED
defconst(env, "sqlite-shm-shared", env->make_integer(env, SQLITE_SHM_SHARED));
#endif
#ifdef SQLITE_SHM_EXCLUSIVE
defconst(env, "sqlite-shm-exclusive", env->make_integer(env, SQLITE_SHM_EXCLUSIVE));
#endif
#ifdef SQLITE_SHM_NLOCK
defconst(env, "sqlite-shm-nlock", env->make_integer(env, SQLITE_SHM_NLOCK));
#endif
#ifdef SQLITE_CONFIG_SINGLETHREAD
defconst(env, "sqlite-config-singlethread", env->make_integer(env, SQLITE_CONFIG_SINGLETHREAD));
#endif
#ifdef SQLITE_CONFIG_MULTITHREAD
defconst(env, "sqlite-config-multithread", env->make_integer(env, SQLITE_CONFIG_MULTITHREAD));
#endif
#ifdef SQLITE_CONFIG_SERIALIZED
defconst(env, "sqlite-config-serialized", env->make_integer(env, SQLITE_CONFIG_SERIALIZED));
#endif
#ifdef SQLITE_CONFIG_MALLOC
defconst(env, "sqlite-config-malloc", env->make_integer(env, SQLITE_CONFIG_MALLOC));
#endif
#ifdef SQLITE_CONFIG_GETMALLOC
defconst(env, "sqlite-config-getmalloc", env->make_integer(env, SQLITE_CONFIG_GETMALLOC));
#endif
#ifdef SQLITE_CONFIG_SCRATCH
defconst(env, "sqlite-config-scratch", env->make_integer(env, SQLITE_CONFIG_SCRATCH));
#endif
#ifdef SQLITE_CONFIG_PAGECACHE
defconst(env, "sqlite-config-pagecache", env->make_integer(env, SQLITE_CONFIG_PAGECACHE));
#endif
#ifdef SQLITE_CONFIG_HEAP
defconst(env, "sqlite-config-heap", env->make_integer(env, SQLITE_CONFIG_HEAP));
#endif
#ifdef SQLITE_CONFIG_MEMSTATUS
defconst(env, "sqlite-config-memstatus", env->make_integer(env, SQLITE_CONFIG_MEMSTATUS));
#endif
#ifdef SQLITE_CONFIG_MUTEX
defconst(env, "sqlite-config-mutex", env->make_integer(env, SQLITE_CONFIG_MUTEX));
#endif
#ifdef SQLITE_CONFIG_GETMUTEX
defconst(env, "sqlite-config-getmutex", env->make_integer(env, SQLITE_CONFIG_GETMUTEX));
#endif
#ifdef SQLITE_CONFIG_LOOKASIDE
defconst(env, "sqlite-config-lookaside", env->make_integer(env, SQLITE_CONFIG_LOOKASIDE));
#endif
#ifdef SQLITE_CONFIG_PCACHE
defconst(env, "sqlite-config-pcache", env->make_integer(env, SQLITE_CONFIG_PCACHE));
#endif
#ifdef SQLITE_CONFIG_GETPCACHE
defconst(env, "sqlite-config-getpcache", env->make_integer(env, SQLITE_CONFIG_GETPCACHE));
#endif
#ifdef SQLITE_CONFIG_LOG
defconst(env, "sqlite-config-log", env->make_integer(env, SQLITE_CONFIG_LOG));
#endif
#ifdef SQLITE_CONFIG_URI
defconst(env, "sqlite-config-uri", env->make_integer(env, SQLITE_CONFIG_URI));
#endif
#ifdef SQLITE_CONFIG_PCACHE2
defconst(env, "sqlite-config-pcache2", env->make_integer(env, SQLITE_CONFIG_PCACHE2));
#endif
#ifdef SQLITE_CONFIG_GETPCACHE2
defconst(env, "sqlite-config-getpcache2", env->make_integer(env, SQLITE_CONFIG_GETPCACHE2));
#endif
#ifdef SQLITE_CONFIG_COVERING_INDEX_SCAN
defconst(env, "sqlite-config-covering-index-scan", env->make_integer(env, SQLITE_CONFIG_COVERING_INDEX_SCAN));
#endif
#ifdef SQLITE_CONFIG_SQLLOG
defconst(env, "sqlite-config-sqllog", env->make_integer(env, SQLITE_CONFIG_SQLLOG));
#endif
#ifdef SQLITE_CONFIG_MMAP_SIZE
defconst(env, "sqlite-config-mmap-size", env->make_integer(env, SQLITE_CONFIG_MMAP_SIZE));
#endif
#ifdef SQLITE_CONFIG_WIN32_HEAPSIZE
defconst(env, "sqlite-config-win32-heapsize", env->make_integer(env, SQLITE_CONFIG_WIN32_HEAPSIZE));
#endif
#ifdef SQLITE_CONFIG_PCACHE_HDRSZ
defconst(env, "sqlite-config-pcache-hdrsz", env->make_integer(env, SQLITE_CONFIG_PCACHE_HDRSZ));
#endif
#ifdef SQLITE_CONFIG_PMASZ
defconst(env, "sqlite-config-pmasz", env->make_integer(env, SQLITE_CONFIG_PMASZ));
#endif
#ifdef SQLITE_CONFIG_STMTJRNL_SPILL
defconst(env, "sqlite-config-stmtjrnl-spill", env->make_integer(env, SQLITE_CONFIG_STMTJRNL_SPILL));
#endif
#ifdef SQLITE_CONFIG_SMALL_MALLOC
defconst(env, "sqlite-config-small-malloc", env->make_integer(env, SQLITE_CONFIG_SMALL_MALLOC));
#endif
#ifdef SQLITE_CONFIG_SORTERREF_SIZE
defconst(env, "sqlite-config-sorterref-size", env->make_integer(env, SQLITE_CONFIG_SORTERREF_SIZE));
#endif
#ifdef SQLITE_CONFIG_MEMDB_MAXSIZE
defconst(env, "sqlite-config-memdb-maxsize", env->make_integer(env, SQLITE_CONFIG_MEMDB_MAXSIZE));
#endif
#ifdef SQLITE_DBCONFIG_MAINDBNAME
defconst(env, "sqlite-dbconfig-maindbname", env->make_integer(env, SQLITE_DBCONFIG_MAINDBNAME));
#endif
#ifdef SQLITE_DBCONFIG_LOOKASIDE
defconst(env, "sqlite-dbconfig-lookaside", env->make_integer(env, SQLITE_DBCONFIG_LOOKASIDE));
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_FKEY
defconst(env, "sqlite-dbconfig-enable-fkey", env->make_integer(env, SQLITE_DBCONFIG_ENABLE_FKEY));
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_TRIGGER
defconst(env, "sqlite-dbconfig-enable-trigger", env->make_integer(env, SQLITE_DBCONFIG_ENABLE_TRIGGER));
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER
defconst(env, "sqlite-dbconfig-enable-fts3-tokenizer", env->make_integer(env, SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER));
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION
defconst(env, "sqlite-dbconfig-enable-load-extension", env->make_integer(env, SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION));
#endif
#ifdef SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE
defconst(env, "sqlite-dbconfig-no-ckpt-on-close", env->make_integer(env, SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE));
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_QPSG
defconst(env, "sqlite-dbconfig-enable-qpsg", env->make_integer(env, SQLITE_DBCONFIG_ENABLE_QPSG));
#endif
#ifdef SQLITE_DBCONFIG_TRIGGER_EQP
defconst(env, "sqlite-dbconfig-trigger-eqp", env->make_integer(env, SQLITE_DBCONFIG_TRIGGER_EQP));
#endif
#ifdef SQLITE_DBCONFIG_RESET_DATABASE
defconst(env, "sqlite-dbconfig-reset-database", env->make_integer(env, SQLITE_DBCONFIG_RESET_DATABASE));
#endif
#ifdef SQLITE_DBCONFIG_DEFENSIVE
defconst(env, "sqlite-dbconfig-defensive", env->make_integer(env, SQLITE_DBCONFIG_DEFENSIVE));
#endif
#ifdef SQLITE_DBCONFIG_WRITABLE_SCHEMA
defconst(env, "sqlite-dbconfig-writable-schema", env->make_integer(env, SQLITE_DBCONFIG_WRITABLE_SCHEMA));
#endif
#ifdef SQLITE_DBCONFIG_LEGACY_ALTER_TABLE
defconst(env, "sqlite-dbconfig-legacy-alter-table", env->make_integer(env, SQLITE_DBCONFIG_LEGACY_ALTER_TABLE));
#endif
#ifdef SQLITE_DBCONFIG_DQS_DML
defconst(env, "sqlite-dbconfig-dqs-dml", env->make_integer(env, SQLITE_DBCONFIG_DQS_DML));
#endif
#ifdef SQLITE_DBCONFIG_DQS_DDL
defconst(env, "sqlite-dbconfig-dqs-ddl", env->make_integer(env, SQLITE_DBCONFIG_DQS_DDL));
#endif
#ifdef SQLITE_DBCONFIG_ENABLE_VIEW
defconst(env, "sqlite-dbconfig-enable-view", env->make_integer(env, SQLITE_DBCONFIG_ENABLE_VIEW));
#endif
#ifdef SQLITE_DBCONFIG_LEGACY_FILE_FORMAT
defconst(env, "sqlite-dbconfig-legacy-file-format", env->make_integer(env, SQLITE_DBCONFIG_LEGACY_FILE_FORMAT));
#endif
#ifdef SQLITE_DBCONFIG_TRUSTED_SCHEMA
defconst(env, "sqlite-dbconfig-trusted-schema", env->make_integer(env, SQLITE_DBCONFIG_TRUSTED_SCHEMA));
#endif
#ifdef SQLITE_DBCONFIG_MAX
defconst(env, "sqlite-dbconfig-max", env->make_integer(env, SQLITE_DBCONFIG_MAX));
#endif
#ifdef SQLITE_DENY
defconst(env, "sqlite-deny", env->make_integer(env, SQLITE_DENY));
#endif
#ifdef SQLITE_IGNORE
defconst(env, "sqlite-ignore", env->make_integer(env, SQLITE_IGNORE));
#endif
#ifdef SQLITE_CREATE_INDEX
defconst(env, "sqlite-create-index", env->make_integer(env, SQLITE_CREATE_INDEX));
#endif
#ifdef SQLITE_CREATE_TABLE
defconst(env, "sqlite-create-table", env->make_integer(env, SQLITE_CREATE_TABLE));
#endif
#ifdef SQLITE_CREATE_TEMP_INDEX
defconst(env, "sqlite-create-temp-index", env->make_integer(env, SQLITE_CREATE_TEMP_INDEX));
#endif
#ifdef SQLITE_CREATE_TEMP_TABLE
defconst(env, "sqlite-create-temp-table", env->make_integer(env, SQLITE_CREATE_TEMP_TABLE));
#endif
#ifdef SQLITE_CREATE_TEMP_TRIGGER
defconst(env, "sqlite-create-temp-trigger", env->make_integer(env, SQLITE_CREATE_TEMP_TRIGGER));
#endif
#ifdef SQLITE_CREATE_TEMP_VIEW
defconst(env, "sqlite-create-temp-view", env->make_integer(env, SQLITE_CREATE_TEMP_VIEW));
#endif
#ifdef SQLITE_CREATE_TRIGGER
defconst(env, "sqlite-create-trigger", env->make_integer(env, SQLITE_CREATE_TRIGGER));
#endif
#ifdef SQLITE_CREATE_VIEW
defconst(env, "sqlite-create-view", env->make_integer(env, SQLITE_CREATE_VIEW));
#endif
#ifdef SQLITE_DELETE
defconst(env, "sqlite-delete", env->make_integer(env, SQLITE_DELETE));
#endif
#ifdef SQLITE_DROP_INDEX
defconst(env, "sqlite-drop-index", env->make_integer(env, SQLITE_DROP_INDEX));
#endif
#ifdef SQLITE_DROP_TABLE
defconst(env, "sqlite-drop-table", env->make_integer(env, SQLITE_DROP_TABLE));
#endif
#ifdef SQLITE_DROP_TEMP_INDEX
defconst(env, "sqlite-drop-temp-index", env->make_integer(env, SQLITE_DROP_TEMP_INDEX));
#endif
#ifdef SQLITE_DROP_TEMP_TABLE
defconst(env, "sqlite-drop-temp-table", env->make_integer(env, SQLITE_DROP_TEMP_TABLE));
#endif
#ifdef SQLITE_DROP_TEMP_TRIGGER
defconst(env, "sqlite-drop-temp-trigger", env->make_integer(env, SQLITE_DROP_TEMP_TRIGGER));
#endif
#ifdef SQLITE_DROP_TEMP_VIEW
defconst(env, "sqlite-drop-temp-view", env->make_integer(env, SQLITE_DROP_TEMP_VIEW));
#endif
#ifdef SQLITE_DROP_TRIGGER
defconst(env, "sqlite-drop-trigger", env->make_integer(env, SQLITE_DROP_TRIGGER));
#endif
#ifdef SQLITE_DROP_VIEW
defconst(env, "sqlite-drop-view", env->make_integer(env, SQLITE_DROP_VIEW));
#endif
#ifdef SQLITE_INSERT
defconst(env, "sqlite-insert", env->make_integer(env, SQLITE_INSERT));
#endif
#ifdef SQLITE_PRAGMA
defconst(env, "sqlite-pragma", env->make_integer(env, SQLITE_PRAGMA));
#endif
#ifdef SQLITE_READ
defconst(env, "sqlite-read", env->make_integer(env, SQLITE_READ));
#endif
#ifdef SQLITE_SELECT
defconst(env, "sqlite-select", env->make_integer(env, SQLITE_SELECT));
#endif
#ifdef SQLITE_TRANSACTION
defconst(env, "sqlite-transaction", env->make_integer(env, SQLITE_TRANSACTION));
#endif
#ifdef SQLITE_UPDATE
defconst(env, "sqlite-update", env->make_integer(env, SQLITE_UPDATE));
#endif
#ifdef SQLITE_ATTACH
defconst(env, "sqlite-attach", env->make_integer(env, SQLITE_ATTACH));
#endif
#ifdef SQLITE_DETACH
defconst(env, "sqlite-detach", env->make_integer(env, SQLITE_DETACH));
#endif
#ifdef SQLITE_ALTER_TABLE
defconst(env, "sqlite-alter-table", env->make_integer(env, SQLITE_ALTER_TABLE));
#endif
#ifdef SQLITE_REINDEX
defconst(env, "sqlite-reindex", env->make_integer(env, SQLITE_REINDEX));
#endif
#ifdef SQLITE_ANALYZE
defconst(env, "sqlite-analyze", env->make_integer(env, SQLITE_ANALYZE));
#endif
#ifdef SQLITE_CREATE_VTABLE
defconst(env, "sqlite-create-vtable", env->make_integer(env, SQLITE_CREATE_VTABLE));
#endif
#ifdef SQLITE_DROP_VTABLE
defconst(env, "sqlite-drop-vtable", env->make_integer(env, SQLITE_DROP_VTABLE));
#endif
#ifdef SQLITE_FUNCTION
defconst(env, "sqlite-function", env->make_integer(env, SQLITE_FUNCTION));
#endif
#ifdef SQLITE_SAVEPOINT
defconst(env, "sqlite-savepoint", env->make_integer(env, SQLITE_SAVEPOINT));
#endif
#ifdef SQLITE_COPY
defconst(env, "sqlite-copy", env->make_integer(env, SQLITE_COPY));
#endif
#ifdef SQLITE_RECURSIVE
defconst(env, "sqlite-recursive", env->make_integer(env, SQLITE_RECURSIVE));
#endif
#ifdef SQLITE_TRACE_STMT
defconst(env, "sqlite-trace-stmt", env->make_integer(env, SQLITE_TRACE_STMT));
#endif
#ifdef SQLITE_TRACE_PROFILE
defconst(env, "sqlite-trace-profile", env->make_integer(env, SQLITE_TRACE_PROFILE));
#endif
#ifdef SQLITE_TRACE_ROW
defconst(env, "sqlite-trace-row", env->make_integer(env, SQLITE_TRACE_ROW));
#endif
#ifdef SQLITE_TRACE_CLOSE
defconst(env, "sqlite-trace-close", env->make_integer(env, SQLITE_TRACE_CLOSE));
#endif
#ifdef SQLITE_LIMIT_LENGTH
defconst(env, "sqlite-limit-length", env->make_integer(env, SQLITE_LIMIT_LENGTH));
#endif
#ifdef SQLITE_LIMIT_SQL_LENGTH
defconst(env, "sqlite-limit-sql-length", env->make_integer(env, SQLITE_LIMIT_SQL_LENGTH));
#endif
#ifdef SQLITE_LIMIT_COLUMN
defconst(env, "sqlite-limit-column", env->make_integer(env, SQLITE_LIMIT_COLUMN));
#endif
#ifdef SQLITE_LIMIT_EXPR_DEPTH
defconst(env, "sqlite-limit-expr-depth", env->make_integer(env, SQLITE_LIMIT_EXPR_DEPTH));
#endif
#ifdef SQLITE_LIMIT_COMPOUND_SELECT
defconst(env, "sqlite-limit-compound-select", env->make_integer(env, SQLITE_LIMIT_COMPOUND_SELECT));
#endif
#ifdef SQLITE_LIMIT_VDBE_OP
defconst(env, "sqlite-limit-vdbe-op", env->make_integer(env, SQLITE_LIMIT_VDBE_OP));
#endif
#ifdef SQLITE_LIMIT_FUNCTION_ARG
defconst(env, "sqlite-limit-function-arg", env->make_integer(env, SQLITE_LIMIT_FUNCTION_ARG));
#endif
#ifdef SQLITE_LIMIT_ATTACHED
defconst(env, "sqlite-limit-attached", env->make_integer(env, SQLITE_LIMIT_ATTACHED));
#endif
#ifdef SQLITE_LIMIT_LIKE_PATTERN_LENGTH
defconst(env, "sqlite-limit-like-pattern-length", env->make_integer(env, SQLITE_LIMIT_LIKE_PATTERN_LENGTH));
#endif
#ifdef SQLITE_LIMIT_VARIABLE_NUMBER
defconst(env, "sqlite-limit-variable-number", env->make_integer(env, SQLITE_LIMIT_VARIABLE_NUMBER));
#endif
#ifdef SQLITE_LIMIT_TRIGGER_DEPTH
defconst(env, "sqlite-limit-trigger-depth", env->make_integer(env, SQLITE_LIMIT_TRIGGER_DEPTH));
#endif
#ifdef SQLITE_LIMIT_WORKER_THREADS
defconst(env, "sqlite-limit-worker-threads", env->make_integer(env, SQLITE_LIMIT_WORKER_THREADS));
#endif
#ifdef SQLITE_PREPARE_PERSISTENT
defconst(env, "sqlite-prepare-persistent", env->make_integer(env, SQLITE_PREPARE_PERSISTENT));
#endif
#ifdef SQLITE_PREPARE_NORMALIZE
defconst(env, "sqlite-prepare-normalize", env->make_integer(env, SQLITE_PREPARE_NORMALIZE));
#endif
#ifdef SQLITE_PREPARE_NO_VTAB
defconst(env, "sqlite-prepare-no-vtab", env->make_integer(env, SQLITE_PREPARE_NO_VTAB));
#endif
#ifdef SQLITE_INTEGER
defconst(env, "sqlite-integer", env->make_integer(env, SQLITE_INTEGER));
#endif
#ifdef SQLITE_FLOAT
defconst(env, "sqlite-float", env->make_integer(env, SQLITE_FLOAT));
#endif
#ifdef SQLITE_BLOB
defconst(env, "sqlite-blob", env->make_integer(env, SQLITE_BLOB));
#endif
#ifdef SQLITE_NULL
defconst(env, "sqlite-null", env->make_integer(env, SQLITE_NULL));
#endif
#ifdef SQLITE_UTF8
defconst(env, "sqlite-utf8", env->make_integer(env, SQLITE_UTF8));
#endif
#ifdef SQLITE_UTF16LE
defconst(env, "sqlite-utf16le", env->make_integer(env, SQLITE_UTF16LE));
#endif
#ifdef SQLITE_UTF16BE
defconst(env, "sqlite-utf16be", env->make_integer(env, SQLITE_UTF16BE));
#endif
#ifdef SQLITE_UTF16
defconst(env, "sqlite-utf16", env->make_integer(env, SQLITE_UTF16));
#endif
#ifdef SQLITE_ANY
defconst(env, "sqlite-any", env->make_integer(env, SQLITE_ANY));
#endif
#ifdef SQLITE_UTF16_ALIGNED
defconst(env, "sqlite-utf16-aligned", env->make_integer(env, SQLITE_UTF16_ALIGNED));
#endif
#ifdef SQLITE_DETERMINISTIC
defconst(env, "sqlite-deterministic", env->make_integer(env, SQLITE_DETERMINISTIC));
#endif
#ifdef SQLITE_DIRECTONLY
defconst(env, "sqlite-directonly", env->make_integer(env, SQLITE_DIRECTONLY));
#endif
#ifdef SQLITE_SUBTYPE
defconst(env, "sqlite-subtype", env->make_integer(env, SQLITE_SUBTYPE));
#endif
#ifdef SQLITE_INNOCUOUS
defconst(env, "sqlite-innocuous", env->make_integer(env, SQLITE_INNOCUOUS));
#endif
#ifdef SQLITE_WIN32_DATA_DIRECTORY_TYPE
defconst(env, "sqlite-win32-data-directory-type", env->make_integer(env, SQLITE_WIN32_DATA_DIRECTORY_TYPE));
#endif
#ifdef SQLITE_WIN32_TEMP_DIRECTORY_TYPE
defconst(env, "sqlite-win32-temp-directory-type", env->make_integer(env, SQLITE_WIN32_TEMP_DIRECTORY_TYPE));
#endif
#ifdef SQLITE_INDEX_SCAN_UNIQUE
defconst(env, "sqlite-index-scan-unique", env->make_integer(env, SQLITE_INDEX_SCAN_UNIQUE));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_EQ
defconst(env, "sqlite-index-constraint-eq", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_EQ));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_GT
defconst(env, "sqlite-index-constraint-gt", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_GT));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_LE
defconst(env, "sqlite-index-constraint-le", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_LE));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_LT
defconst(env, "sqlite-index-constraint-lt", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_LT));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_GE
defconst(env, "sqlite-index-constraint-ge", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_GE));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_MATCH
defconst(env, "sqlite-index-constraint-match", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_MATCH));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_LIKE
defconst(env, "sqlite-index-constraint-like", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_LIKE));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_GLOB
defconst(env, "sqlite-index-constraint-glob", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_GLOB));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_REGEXP
defconst(env, "sqlite-index-constraint-regexp", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_REGEXP));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_NE
defconst(env, "sqlite-index-constraint-ne", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_NE));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_ISNOT
defconst(env, "sqlite-index-constraint-isnot", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_ISNOT));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_ISNOTNULL
defconst(env, "sqlite-index-constraint-isnotnull", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_ISNOTNULL));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_ISNULL
defconst(env, "sqlite-index-constraint-isnull", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_ISNULL));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_IS
defconst(env, "sqlite-index-constraint-is", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_IS));
#endif
#ifdef SQLITE_INDEX_CONSTRAINT_FUNCTION
defconst(env, "sqlite-index-constraint-function", env->make_integer(env, SQLITE_INDEX_CONSTRAINT_FUNCTION));
#endif
#ifdef SQLITE_MUTEX_FAST
defconst(env, "sqlite-mutex-fast", env->make_integer(env, SQLITE_MUTEX_FAST));
#endif
#ifdef SQLITE_MUTEX_RECURSIVE
defconst(env, "sqlite-mutex-recursive", env->make_integer(env, SQLITE_MUTEX_RECURSIVE));
#endif
#ifdef SQLITE_MUTEX_STATIC_MASTER
defconst(env, "sqlite-mutex-static-master", env->make_integer(env, SQLITE_MUTEX_STATIC_MASTER));
#endif
#ifdef SQLITE_MUTEX_STATIC_MEM
defconst(env, "sqlite-mutex-static-mem", env->make_integer(env, SQLITE_MUTEX_STATIC_MEM));
#endif
#ifdef SQLITE_MUTEX_STATIC_MEM2
defconst(env, "sqlite-mutex-static-mem2", env->make_integer(env, SQLITE_MUTEX_STATIC_MEM2));
#endif
#ifdef SQLITE_MUTEX_STATIC_OPEN
defconst(env, "sqlite-mutex-static-open", env->make_integer(env, SQLITE_MUTEX_STATIC_OPEN));
#endif
#ifdef SQLITE_MUTEX_STATIC_PRNG
defconst(env, "sqlite-mutex-static-prng", env->make_integer(env, SQLITE_MUTEX_STATIC_PRNG));
#endif
#ifdef SQLITE_MUTEX_STATIC_LRU
defconst(env, "sqlite-mutex-static-lru", env->make_integer(env, SQLITE_MUTEX_STATIC_LRU));
#endif
#ifdef SQLITE_MUTEX_STATIC_LRU2
defconst(env, "sqlite-mutex-static-lru2", env->make_integer(env, SQLITE_MUTEX_STATIC_LRU2));
#endif
#ifdef SQLITE_MUTEX_STATIC_PMEM
defconst(env, "sqlite-mutex-static-pmem", env->make_integer(env, SQLITE_MUTEX_STATIC_PMEM));
#endif
#ifdef SQLITE_MUTEX_STATIC_APP1
defconst(env, "sqlite-mutex-static-app1", env->make_integer(env, SQLITE_MUTEX_STATIC_APP1));
#endif
#ifdef SQLITE_MUTEX_STATIC_APP2
defconst(env, "sqlite-mutex-static-app2", env->make_integer(env, SQLITE_MUTEX_STATIC_APP2));
#endif
#ifdef SQLITE_MUTEX_STATIC_APP3
defconst(env, "sqlite-mutex-static-app3", env->make_integer(env, SQLITE_MUTEX_STATIC_APP3));
#endif
#ifdef SQLITE_MUTEX_STATIC_VFS1
defconst(env, "sqlite-mutex-static-vfs1", env->make_integer(env, SQLITE_MUTEX_STATIC_VFS1));
#endif
#ifdef SQLITE_MUTEX_STATIC_VFS2
defconst(env, "sqlite-mutex-static-vfs2", env->make_integer(env, SQLITE_MUTEX_STATIC_VFS2));
#endif
#ifdef SQLITE_MUTEX_STATIC_VFS3
defconst(env, "sqlite-mutex-static-vfs3", env->make_integer(env, SQLITE_MUTEX_STATIC_VFS3));
#endif
#ifdef SQLITE_TESTCTRL_FIRST
defconst(env, "sqlite-testctrl-first", env->make_integer(env, SQLITE_TESTCTRL_FIRST));
#endif
#ifdef SQLITE_TESTCTRL_PRNG_SAVE
defconst(env, "sqlite-testctrl-prng-save", env->make_integer(env, SQLITE_TESTCTRL_PRNG_SAVE));
#endif
#ifdef SQLITE_TESTCTRL_PRNG_RESTORE
defconst(env, "sqlite-testctrl-prng-restore", env->make_integer(env, SQLITE_TESTCTRL_PRNG_RESTORE));
#endif
#ifdef SQLITE_TESTCTRL_PRNG_RESET
defconst(env, "sqlite-testctrl-prng-reset", env->make_integer(env, SQLITE_TESTCTRL_PRNG_RESET));
#endif
#ifdef SQLITE_TESTCTRL_BITVEC_TEST
defconst(env, "sqlite-testctrl-bitvec-test", env->make_integer(env, SQLITE_TESTCTRL_BITVEC_TEST));
#endif
#ifdef SQLITE_TESTCTRL_FAULT_INSTALL
defconst(env, "sqlite-testctrl-fault-install", env->make_integer(env, SQLITE_TESTCTRL_FAULT_INSTALL));
#endif
#ifdef SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS
defconst(env, "sqlite-testctrl-benign-malloc-hooks", env->make_integer(env, SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS));
#endif
#ifdef SQLITE_TESTCTRL_PENDING_BYTE
defconst(env, "sqlite-testctrl-pending-byte", env->make_integer(env, SQLITE_TESTCTRL_PENDING_BYTE));
#endif
#ifdef SQLITE_TESTCTRL_ASSERT
defconst(env, "sqlite-testctrl-assert", env->make_integer(env, SQLITE_TESTCTRL_ASSERT));
#endif
#ifdef SQLITE_TESTCTRL_ALWAYS
defconst(env, "sqlite-testctrl-always", env->make_integer(env, SQLITE_TESTCTRL_ALWAYS));
#endif
#ifdef SQLITE_TESTCTRL_RESERVE
defconst(env, "sqlite-testctrl-reserve", env->make_integer(env, SQLITE_TESTCTRL_RESERVE));
#endif
#ifdef SQLITE_TESTCTRL_OPTIMIZATIONS
defconst(env, "sqlite-testctrl-optimizations", env->make_integer(env, SQLITE_TESTCTRL_OPTIMIZATIONS));
#endif
#ifdef SQLITE_TESTCTRL_ISKEYWORD
defconst(env, "sqlite-testctrl-iskeyword", env->make_integer(env, SQLITE_TESTCTRL_ISKEYWORD));
#endif
#ifdef SQLITE_TESTCTRL_SCRATCHMALLOC
defconst(env, "sqlite-testctrl-scratchmalloc", env->make_integer(env, SQLITE_TESTCTRL_SCRATCHMALLOC));
#endif
#ifdef SQLITE_TESTCTRL_INTERNAL_FUNCTIONS
defconst(env, "sqlite-testctrl-internal-functions", env->make_integer(env, SQLITE_TESTCTRL_INTERNAL_FUNCTIONS));
#endif
#ifdef SQLITE_TESTCTRL_LOCALTIME_FAULT
defconst(env, "sqlite-testctrl-localtime-fault", env->make_integer(env, SQLITE_TESTCTRL_LOCALTIME_FAULT));
#endif
#ifdef SQLITE_TESTCTRL_EXPLAIN_STMT
defconst(env, "sqlite-testctrl-explain-stmt", env->make_integer(env, SQLITE_TESTCTRL_EXPLAIN_STMT));
#endif
#ifdef SQLITE_TESTCTRL_ONCE_RESET_THRESHOLD
defconst(env, "sqlite-testctrl-once-reset-threshold", env->make_integer(env, SQLITE_TESTCTRL_ONCE_RESET_THRESHOLD));
#endif
#ifdef SQLITE_TESTCTRL_NEVER_CORRUPT
defconst(env, "sqlite-testctrl-never-corrupt", env->make_integer(env, SQLITE_TESTCTRL_NEVER_CORRUPT));
#endif
#ifdef SQLITE_TESTCTRL_VDBE_COVERAGE
defconst(env, "sqlite-testctrl-vdbe-coverage", env->make_integer(env, SQLITE_TESTCTRL_VDBE_COVERAGE));
#endif
#ifdef SQLITE_TESTCTRL_BYTEORDER
defconst(env, "sqlite-testctrl-byteorder", env->make_integer(env, SQLITE_TESTCTRL_BYTEORDER));
#endif
#ifdef SQLITE_TESTCTRL_ISINIT
defconst(env, "sqlite-testctrl-isinit", env->make_integer(env, SQLITE_TESTCTRL_ISINIT));
#endif
#ifdef SQLITE_TESTCTRL_SORTER_MMAP
defconst(env, "sqlite-testctrl-sorter-mmap", env->make_integer(env, SQLITE_TESTCTRL_SORTER_MMAP));
#endif
#ifdef SQLITE_TESTCTRL_IMPOSTER
defconst(env, "sqlite-testctrl-imposter", env->make_integer(env, SQLITE_TESTCTRL_IMPOSTER));
#endif
#ifdef SQLITE_TESTCTRL_PARSER_COVERAGE
defconst(env, "sqlite-testctrl-parser-coverage", env->make_integer(env, SQLITE_TESTCTRL_PARSER_COVERAGE));
#endif
#ifdef SQLITE_TESTCTRL_RESULT_INTREAL
defconst(env, "sqlite-testctrl-result-intreal", env->make_integer(env, SQLITE_TESTCTRL_RESULT_INTREAL));
#endif
#ifdef SQLITE_TESTCTRL_PRNG_SEED
defconst(env, "sqlite-testctrl-prng-seed", env->make_integer(env, SQLITE_TESTCTRL_PRNG_SEED));
#endif
#ifdef SQLITE_TESTCTRL_EXTRA_SCHEMA_CHECKS
defconst(env, "sqlite-testctrl-extra-schema-checks", env->make_integer(env, SQLITE_TESTCTRL_EXTRA_SCHEMA_CHECKS));
#endif
#ifdef SQLITE_TESTCTRL_LAST
defconst(env, "sqlite-testctrl-last", env->make_integer(env, SQLITE_TESTCTRL_LAST));
#endif
#ifdef SQLITE_STATUS_MEMORY_USED
defconst(env, "sqlite-status-memory-used", env->make_integer(env, SQLITE_STATUS_MEMORY_USED));
#endif
#ifdef SQLITE_STATUS_PAGECACHE_USED
defconst(env, "sqlite-status-pagecache-used", env->make_integer(env, SQLITE_STATUS_PAGECACHE_USED));
#endif
#ifdef SQLITE_STATUS_PAGECACHE_OVERFLOW
defconst(env, "sqlite-status-pagecache-overflow", env->make_integer(env, SQLITE_STATUS_PAGECACHE_OVERFLOW));
#endif
#ifdef SQLITE_STATUS_SCRATCH_USED
defconst(env, "sqlite-status-scratch-used", env->make_integer(env, SQLITE_STATUS_SCRATCH_USED));
#endif
#ifdef SQLITE_STATUS_SCRATCH_OVERFLOW
defconst(env, "sqlite-status-scratch-overflow", env->make_integer(env, SQLITE_STATUS_SCRATCH_OVERFLOW));
#endif
#ifdef SQLITE_STATUS_MALLOC_SIZE
defconst(env, "sqlite-status-malloc-size", env->make_integer(env, SQLITE_STATUS_MALLOC_SIZE));
#endif
#ifdef SQLITE_STATUS_PARSER_STACK
defconst(env, "sqlite-status-parser-stack", env->make_integer(env, SQLITE_STATUS_PARSER_STACK));
#endif
#ifdef SQLITE_STATUS_PAGECACHE_SIZE
defconst(env, "sqlite-status-pagecache-size", env->make_integer(env, SQLITE_STATUS_PAGECACHE_SIZE));
#endif
#ifdef SQLITE_STATUS_SCRATCH_SIZE
defconst(env, "sqlite-status-scratch-size", env->make_integer(env, SQLITE_STATUS_SCRATCH_SIZE));
#endif
#ifdef SQLITE_STATUS_MALLOC_COUNT
defconst(env, "sqlite-status-malloc-count", env->make_integer(env, SQLITE_STATUS_MALLOC_COUNT));
#endif
#ifdef SQLITE_DBSTATUS_LOOKASIDE_USED
defconst(env, "sqlite-dbstatus-lookaside-used", env->make_integer(env, SQLITE_DBSTATUS_LOOKASIDE_USED));
#endif
#ifdef SQLITE_DBSTATUS_CACHE_USED
defconst(env, "sqlite-dbstatus-cache-used", env->make_integer(env, SQLITE_DBSTATUS_CACHE_USED));
#endif
#ifdef SQLITE_DBSTATUS_SCHEMA_USED
defconst(env, "sqlite-dbstatus-schema-used", env->make_integer(env, SQLITE_DBSTATUS_SCHEMA_USED));
#endif
#ifdef SQLITE_DBSTATUS_STMT_USED
defconst(env, "sqlite-dbstatus-stmt-used", env->make_integer(env, SQLITE_DBSTATUS_STMT_USED));
#endif
#ifdef SQLITE_DBSTATUS_LOOKASIDE_HIT
defconst(env, "sqlite-dbstatus-lookaside-hit", env->make_integer(env, SQLITE_DBSTATUS_LOOKASIDE_HIT));
#endif
#ifdef SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE
defconst(env, "sqlite-dbstatus-lookaside-miss-size", env->make_integer(env, SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE));
#endif
#ifdef SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL
defconst(env, "sqlite-dbstatus-lookaside-miss-full", env->make_integer(env, SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL));
#endif
#ifdef SQLITE_DBSTATUS_CACHE_HIT
defconst(env, "sqlite-dbstatus-cache-hit", env->make_integer(env, SQLITE_DBSTATUS_CACHE_HIT));
#endif
#ifdef SQLITE_DBSTATUS_CACHE_MISS
defconst(env, "sqlite-dbstatus-cache-miss", env->make_integer(env, SQLITE_DBSTATUS_CACHE_MISS));
#endif
#ifdef SQLITE_DBSTATUS_CACHE_WRITE
defconst(env, "sqlite-dbstatus-cache-write", env->make_integer(env, SQLITE_DBSTATUS_CACHE_WRITE));
#endif
#ifdef SQLITE_DBSTATUS_DEFERRED_FKS
defconst(env, "sqlite-dbstatus-deferred-fks", env->make_integer(env, SQLITE_DBSTATUS_DEFERRED_FKS));
#endif
#ifdef SQLITE_DBSTATUS_CACHE_USED_SHARED
defconst(env, "sqlite-dbstatus-cache-used-shared", env->make_integer(env, SQLITE_DBSTATUS_CACHE_USED_SHARED));
#endif
#ifdef SQLITE_DBSTATUS_CACHE_SPILL
defconst(env, "sqlite-dbstatus-cache-spill", env->make_integer(env, SQLITE_DBSTATUS_CACHE_SPILL));
#endif
#ifdef SQLITE_DBSTATUS_MAX
defconst(env, "sqlite-dbstatus-max", env->make_integer(env, SQLITE_DBSTATUS_MAX));
#endif
#ifdef SQLITE_STMTSTATUS_FULLSCAN_STEP
defconst(env, "sqlite-stmtstatus-fullscan-step", env->make_integer(env, SQLITE_STMTSTATUS_FULLSCAN_STEP));
#endif
#ifdef SQLITE_STMTSTATUS_SORT
defconst(env, "sqlite-stmtstatus-sort", env->make_integer(env, SQLITE_STMTSTATUS_SORT));
#endif
#ifdef SQLITE_STMTSTATUS_AUTOINDEX
defconst(env, "sqlite-stmtstatus-autoindex", env->make_integer(env, SQLITE_STMTSTATUS_AUTOINDEX));
#endif
#ifdef SQLITE_STMTSTATUS_VM_STEP
defconst(env, "sqlite-stmtstatus-vm-step", env->make_integer(env, SQLITE_STMTSTATUS_VM_STEP));
#endif
#ifdef SQLITE_STMTSTATUS_REPREPARE
defconst(env, "sqlite-stmtstatus-reprepare", env->make_integer(env, SQLITE_STMTSTATUS_REPREPARE));
#endif
#ifdef SQLITE_STMTSTATUS_RUN
defconst(env, "sqlite-stmtstatus-run", env->make_integer(env, SQLITE_STMTSTATUS_RUN));
#endif
#ifdef SQLITE_STMTSTATUS_MEMUSED
defconst(env, "sqlite-stmtstatus-memused", env->make_integer(env, SQLITE_STMTSTATUS_MEMUSED));
#endif
#ifdef SQLITE_CHECKPOINT_PASSIVE
defconst(env, "sqlite-checkpoint-passive", env->make_integer(env, SQLITE_CHECKPOINT_PASSIVE));
#endif
#ifdef SQLITE_CHECKPOINT_FULL
defconst(env, "sqlite-checkpoint-full", env->make_integer(env, SQLITE_CHECKPOINT_FULL));
#endif
#ifdef SQLITE_CHECKPOINT_RESTART
defconst(env, "sqlite-checkpoint-restart", env->make_integer(env, SQLITE_CHECKPOINT_RESTART));
#endif
#ifdef SQLITE_CHECKPOINT_TRUNCATE
defconst(env, "sqlite-checkpoint-truncate", env->make_integer(env, SQLITE_CHECKPOINT_TRUNCATE));
#endif
#ifdef SQLITE_VTAB_CONSTRAINT_SUPPORT
defconst(env, "sqlite-vtab-constraint-support", env->make_integer(env, SQLITE_VTAB_CONSTRAINT_SUPPORT));
#endif
#ifdef SQLITE_VTAB_INNOCUOUS
defconst(env, "sqlite-vtab-innocuous", env->make_integer(env, SQLITE_VTAB_INNOCUOUS));
#endif
#ifdef SQLITE_VTAB_DIRECTONLY
defconst(env, "sqlite-vtab-directonly", env->make_integer(env, SQLITE_VTAB_DIRECTONLY));
#endif
#ifdef SQLITE_ROLLBACK
defconst(env, "sqlite-rollback", env->make_integer(env, SQLITE_ROLLBACK));
#endif
#ifdef SQLITE_FAIL
defconst(env, "sqlite-fail", env->make_integer(env, SQLITE_FAIL));
#endif
#ifdef SQLITE_REPLACE
defconst(env, "sqlite-replace", env->make_integer(env, SQLITE_REPLACE));
#endif
#ifdef SQLITE_SERIALIZE_NOCOPY
defconst(env, "sqlite-serialize-nocopy", env->make_integer(env, SQLITE_SERIALIZE_NOCOPY));
#endif
#ifdef SQLITE_DESERIALIZE_FREEONCLOSE
defconst(env, "sqlite-deserialize-freeonclose", env->make_integer(env, SQLITE_DESERIALIZE_FREEONCLOSE));
#endif
#ifdef SQLITE_DESERIALIZE_RESIZEABLE
defconst(env, "sqlite-deserialize-resizeable", env->make_integer(env, SQLITE_DESERIALIZE_RESIZEABLE));
#endif
#ifdef SQLITE_DESERIALIZE_READONLY
defconst(env, "sqlite-deserialize-readonly", env->make_integer(env, SQLITE_DESERIALIZE_READONLY));
#endif
