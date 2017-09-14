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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sqlite3.h>
#include <float.h>
#include "emacs-module.h"

int plugin_is_GPL_compatible;

#define NON_LOCAL_EXIT_CHECK(env) \
  if ((env)->non_local_exit_check(env) != emacs_funcall_exit_return) {  \
    return (env)->intern((env), "nil");                                 \
  }
#define SYM(env, sym) (env)->intern((env), sym)
#define IS_INTEGER(env, val) \
  (env)->type_of((env), (val)) == (env)->intern((env), "integer")
#define IS_FLOAT(env, val) \
  (env)->type_of((env), (val)) == (env)->intern((env), "float")
#define IS_STRING(env, val) \
  (env)->type_of((env), (val)) == (env)->intern((env), "string")

#define WARN(env, ...) message(env, SQLITE3_LOG_LEVEL_WARN, __VA_ARGS__)
#define DEBUG(env, ...) message(env, SQLITE3_LOG_LEVEL_DEBUG, __VA_ARGS__)
#define ERROR(env, ...) message(env, SQLITE3_LOG_LEVEL_ERROR, __VA_ARGS__)
#define INFO(env, ...) message(env, SQLITE3_LOG_LEVEL_INFO, __VA_ARGS__)

#define FREE(p) if ((p) != 0) free(p);

#define SQLITE3_MAX_LOG_BUF 1000

static int SQLITE3_LOG_LEVEL_DEBUG = 0;
static int SQLITE3_LOG_LEVEL_INFO = 1;
static int SQLITE3_LOG_LEVEL_WARN = 2;
static int SQLITE3_LOG_LEVEL_ERROR = 3;
static int sqlite3_api_log_level;

#if 0
static int symbol_value_as_int(emacs_env *env,
                               emacs_value sym,
                               int deft) {
  emacs_value v = env->funcall(env, SYM(env, "symbol-value"), 1, &sym);
  if (IS_INTEGER(env, v))
    return env->extract_integer(env, v);
  return deft;
}
#endif

/* Equivalent to (list a b c) in elisp
   n is the number of arguments
   elts, an array of emacs_valuem, are elements of the list
 */
static emacs_value make_list(emacs_env *env, int n, emacs_value *elts) {
  return env->funcall(env, SYM(env, "list"), n, elts);
}

#if 0
static void message(emacs_env *env, int log_level, const char *fmt, ...) {
  if (log_level < sqlite3_api_log_level)
    return;

  va_list args;
  static char log_buf[SQLITE3_MAX_LOG_BUF];

  va_start(args, fmt);
  vsnprintf(log_buf, SQLITE3_MAX_LOG_BUF, fmt, args);
  va_end(args);

  static const char *LOG_LEVEL_DESC[] = {
    "DEBUG",
    "INFO",
    "WARN",
    "ERROR"
  };

  static char new_log_buf[SQLITE3_MAX_LOG_BUF];
  snprintf(new_log_buf, SQLITE3_MAX_LOG_BUF, "[%s] %s",
           LOG_LEVEL_DESC[log_level], log_buf);
  emacs_value msg_func = SYM(env, "message");
  emacs_value arg = env->make_string(env, new_log_buf,
                                     strlen(new_log_buf));
  env->funcall(env, msg_func, 1, &arg);
}
#endif

/* Logging function */
static void message(emacs_env *env, int log_level, const char *fmt, ...) {
  (void)env;

  if (log_level < sqlite3_api_log_level)
    return;

  static const char *LOG_LEVEL_DESC[] = {
    "DEBUG",
    "INFO",
    "WARN",
    "ERROR"
  };
  fprintf(stderr, "[%s] ", LOG_LEVEL_DESC[log_level]);

  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);

  fprintf(stderr, "\n");
}

/* Signal an error condition.
   Equivalent to (signal symbol '(msg code)) in elisp */
void signal_error(
    emacs_env *env,
    const char *symbol,
    const char *msg,
    int code) {
  emacs_value signal = SYM(env, symbol);
  emacs_value argv[2] = {
    env->make_string(env, msg, strlen(msg)),
    env->make_integer(env, code)
  };

  env->non_local_exit_signal(
      env,
      signal,
      make_list(env, 2, argv));
}

/* Extract and copy string contents from function parameters */
int extract_string_arg(emacs_env *env, emacs_value arg, char **str) {
  ptrdiff_t size = 0;
  if (!env->copy_string_contents(env, arg, NULL, &size))
    return 1;

  *str = malloc(size);
  if (!env->copy_string_contents(env, arg, *str, &size)) {
    FREE(*str);
    *str = 0;
    return 1;
  }
  return 0;
}

/* Bind the supplied function to a symbol by calling (fset ....) */
void bind_func(emacs_env *env, const char *name, ptrdiff_t min,
               ptrdiff_t max,
               emacs_value (*function) (emacs_env *env,
                                        ptrdiff_t nargs,
                                        emacs_value args[],
                                        void *) EMACS_NOEXCEPT,
               const char *doc) {
  emacs_value fset = SYM(env, "fset");
  emacs_value args[2];

  args[0] = SYM(env, name);
  args[1] = env->make_function(env, min, max, function, doc, 0);
  env->funcall(env, fset, 2, args);
}

/* finalizer for database handle (sqlite3 *) */
static void sqlite3_dbh_gc(void *ptr) {
  INFO(0, "%s: entered", __func__);

  if (ptr) {
    INFO(0, "%s: non-null dbh", __func__);
    sqlite3_close_v2((sqlite3 *)ptr);
  }
}

/* finalizer for statement handle (sqlite3_stmt *) */
static void sqlite3_stmt_gc(void *ptr) {
  INFO(0, "%s: entered", __func__);

  if (ptr) {
    INFO(0, "%s: non-null stmt", __func__);
    sqlite3_finalize((sqlite3_stmt *)ptr);
  }
}

/* bind_*() functions:
   Bind SQL parameters after the SQL is prepared (compiled).
*/
static emacs_value sqlite3_api_bind_null(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_bind_null(stmt, col));
}

static emacs_value sqlite3_api_bind_double(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  double val = env->extract_float(env, args[2]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_bind_double(stmt, col, val));
}

static emacs_value sqlite3_api_bind_parameter_count(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_bind_parameter_count(stmt));
}

static emacs_value sqlite3_api_bind_int64(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);


  intmax_t val = env->extract_integer(env, args[2]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_bind_int64(stmt, col, val));
}

static emacs_value sqlite3_api_bind_text(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  char *txt;
  if (extract_string_arg(env, args[2], &txt)) {
    return SYM(env, "nil");
  }

  DEBUG(env, "%s: [%s] to col %d", __func__, txt, col);
  int rv = sqlite3_bind_text(stmt, col, txt, -1, SQLITE_TRANSIENT);
  FREE(txt);
  return env->make_integer(env, rv);
}

static emacs_value sqlite3_api_bind_multi(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  int rv;
  for (int i = 1; i < n; i++) {
    char *txt = 0;

    //emacs_value type = env->type_of(env, args[i]);
    if (IS_INTEGER(env, args[i])) {
      rv = sqlite3_bind_int64(stmt, i, env->extract_integer(env, args[i]));
      NON_LOCAL_EXIT_CHECK(env);
    } else if (IS_FLOAT(env, args[i])) {
      rv = sqlite3_bind_double(stmt, i, env->extract_float(env, args[i]));
      NON_LOCAL_EXIT_CHECK(env);
    } else if (IS_STRING(env, args[i])) {
      extract_string_arg(env, args[i], &txt);
      rv = sqlite3_bind_text(stmt, i, txt, -1, SQLITE_TRANSIENT);
      FREE(txt);
      NON_LOCAL_EXIT_CHECK(env);
    } else if (args[i] == SYM(env, "nil")) {
      rv = sqlite3_bind_null(stmt, i);
      NON_LOCAL_EXIT_CHECK(env);
    } else {
      WARN(env, "%s: arg %d is of unknown type", __func__, i);
      rv = SQLITE_MISUSE;
    }
    if (rv != SQLITE_OK) {
      WARN(env, "%s: ERROR CODE = %d", __func__, rv);
      sqlite3_reset(stmt);
      break;
    }
  }

  return env->make_integer(env, rv);
}

static emacs_value sqlite3_api_column_name(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  const char *name = sqlite3_column_name(stmt, col);
  return env->make_string(env, name, strlen(name));
}

static emacs_value sqlite3_api_column_text(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  int size = sqlite3_column_bytes(stmt, col);
  return env->make_string(env,
                          (const char *)sqlite3_column_text(stmt, col),
                          size);
}

static emacs_value sqlite3_api_column_int64(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, (intmax_t)sqlite3_column_int64(stmt, col));
}

static emacs_value sqlite3_api_column_double(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_float(env, sqlite3_column_double(stmt, col));
}

static emacs_value sqlite3_api_column_type(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // The column no.
  int col = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_column_type(stmt, col));
}

static emacs_value sqlite3_api_changes(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: database handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3 *dbh = (sqlite3 *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_changes(dbh));
}

static emacs_value sqlite3_api_step(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env)

  return env->make_integer(env, sqlite3_step(stmt));
}

static emacs_value sqlite3_api_reset(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  // Exrtract sqlite3 db struct
  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_reset(stmt));
}

static emacs_value sqlite3_api_column_count(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, sqlite3_column_count(stmt));
}

static emacs_value sqlite3_api_fetch(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: statement handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  /* Create a list to store the results */
  int ncols = sqlite3_column_count(stmt);
  emacs_value *elts = malloc(sizeof(emacs_value)*ncols);
  for (int i = 0; i < ncols; i++) {
    switch(sqlite3_column_type(stmt, i)) {
      case SQLITE_INTEGER:
        elts[i] = env->make_integer(env, sqlite3_column_int64(stmt, i));
        break;
      case SQLITE_FLOAT:
        elts[i] = env->make_float(env, sqlite3_column_double(stmt, i));
        break;
      case SQLITE_TEXT:
        elts[i] = env->make_string(
            env,
            (const char *)sqlite3_column_text(stmt, i),
            sqlite3_column_bytes(stmt, i));
        break;
      default:
        elts[i] = SYM(env, "nil");
    }
  }

  emacs_value res = make_list(env, ncols, elts);
  FREE(elts);
  return res;
}

static emacs_value sqlite3_api_prepare(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: database handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3 *dbh = (sqlite3 *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  // SQL statement to be prepared
  char *sql_txt;
  if (extract_string_arg(env, args[1], &sql_txt)) {
    return SYM(env, "nil");
  }

  // Prepare
  sqlite3_stmt *stmt;
  const char *tail;
  int rv = sqlite3_prepare_v2(dbh, sql_txt, -1, &stmt, &tail);
  INFO(env, "%s: statement prepared (rv=%d)", __func__,  rv);

  FREE(sql_txt);
  if (rv != SQLITE_OK) {
    signal_error(env, "sql-error", "sqlite3_prepare_v2() failed", rv);
    return SYM(env, "nil");
  }
  return env->make_user_ptr(env, sqlite3_stmt_gc, stmt);
}

static emacs_value sqlite3_api_get_autocommit(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  /* User passed a nil stmt */
  if (!env->is_not_nil(env, args[0]))
    return SYM(env, "nil");

  sqlite3 *dbh = (sqlite3 *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  INFO(env, "%s: entered", __func__);
  return env->make_integer(env, sqlite3_get_autocommit(dbh));
}

/* Small struct for passing data from sqlite3_exec() to exec_callback() */
struct func_env {
  emacs_env *env;
  emacs_value callback;
};

/* this #define is only used in exec_callback() */
#define NON_LOCAL_EXIT_CHECK_AND_CLEANUP \
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {    \
  FREE(data_args); \
  FREE(col_args);  \
  return 1; \
}

static int exec_callback(void *data, int ncols,
                         char **col_data, char **col_names) {
  struct func_env *fe = (struct func_env *)data;

  emacs_env *env = fe->env;

  /* Build up two lists and pass them to the LISP callback */
  emacs_value *data_args = malloc(sizeof(emacs_value)*ncols);
  emacs_value *col_args = malloc(sizeof(emacs_value)*ncols);

  for (int i = 0; i < ncols; i++) {
    if (col_data[i])
      data_args[i] = env->make_string(env, col_data[i],
                                      strlen(col_data[i]));
    else
      data_args[i] = SYM(env, "nil");
    NON_LOCAL_EXIT_CHECK_AND_CLEANUP;

    col_args[i] = env->make_string(env, col_names[i],
                                   strlen(col_names[i]));
    NON_LOCAL_EXIT_CHECK_AND_CLEANUP;
  }

  /* equivalent to (list "a" "b" "c" ....) */
  emacs_value args[3];
  args[0] = env->make_integer(env, ncols);
  NON_LOCAL_EXIT_CHECK_AND_CLEANUP;
  args[1] = make_list(env, ncols, data_args);
  NON_LOCAL_EXIT_CHECK_AND_CLEANUP;
  args[2] = make_list(env, ncols, col_args);
  NON_LOCAL_EXIT_CHECK_AND_CLEANUP;

  emacs_value v = env->funcall(env, fe->callback, 3, args);
  FREE(data_args);
  FREE(col_args);

  if (env->is_not_nil(env, v))
    return 0;
  return 1;
}

static emacs_value sqlite3_api_exec(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;

  /* User passed a nil dbh */
  if (!env->is_not_nil(env, args[0]))
    return SYM(env, "nil");

  sqlite3 *dbh = (sqlite3 *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  char *sql_txt;
  if (extract_string_arg(env, args[1], &sql_txt)) {
      FREE(sql_txt);
    return SYM(env, "nil");
  }

  char *errmsg = 0;
  int rv;
  if (n == 3) {
    struct func_env fe = { env, args[2] };
    rv = sqlite3_exec(dbh, sql_txt, exec_callback, (void *)&fe, &errmsg);
  } else {
    rv = sqlite3_exec(dbh, sql_txt, 0, 0, &errmsg);
  }
  FREE(sql_txt);

  if (rv != SQLITE_OK) {
    signal_error(env, "db-error", errmsg, rv);
    if (errmsg)
      sqlite3_free(errmsg);
    return SYM(env, "nil");
  }

  return env->make_integer(env, rv);
}


static emacs_value sqlite3_api_finalize(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;

  INFO(env, "%s: entered", __func__);

  for (int i = 0; i < n; i++) {
    /* User passed a nil stmt */
    if (!env->is_not_nil(env, args[i]))
      /* return SYM(env, "nil"); */
      continue;

    sqlite3_stmt *stmt = (sqlite3_stmt *)env->get_user_ptr(env, args[i]);
    NON_LOCAL_EXIT_CHECK(env);

    sqlite3_finalize(stmt);
    env->set_user_ptr(env, args[i], 0);
    DEBUG(env, "%s: #%d finalized", __func__, i);
  }

  return SYM(env, "nil");
}


static emacs_value sqlite3_api_close(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  /* nil database handle */
  if (!env->is_not_nil(env, args[0]))
    return SYM(env, "nil");

  sqlite3 *dbh = (sqlite3 *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  INFO(env, "%s: entered", __func__);
  sqlite3_close_v2(dbh);
  env->set_user_ptr(env, args[0], 0);
  return SYM(env, "nil");
}

static emacs_value sqlite3_api_last_insert_rowid(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: database handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3 *dbh = (sqlite3 *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  return env->make_integer(env, (intmax_t)sqlite3_last_insert_rowid(dbh));
}


#if 0
/* sqlite version >= 3.18 only */
static emacs_value sqlite3_api_set_last_insert_rowid(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  if (!env->is_not_nil(env, args[0])) {
    WARN(env, "%s: database handle is nil", __func__);
    return SYM(env, "nil");
  }

  sqlite3 *dbh = (sqlite3 *)env->get_user_ptr(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);

  sqlite3_int64 rowid = env->extract_integer(env, args[1]);
  NON_LOCAL_EXIT_CHECK(env);

  sqlite3_set_last_insert_rowid(dbh, rowid);

  return SYM(env, "nil");
}
#endif

static emacs_value sqlite3_api_set_log_level(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  int log_level = env->extract_integer(env, args[0]);
  NON_LOCAL_EXIT_CHECK(env);
  sqlite3_api_log_level = log_level;
  return SYM(env, "nil");
}

#if 0
static emacs_value sqlite3_api_test(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  emacs_value *fargs = malloc(sizeof(emacs_value)*2);
  fargs[0] = env->make_integer(env, 1);
  fargs[1] = env->make_integer(env, 99);
  return env->funcall(env, args[0], 2, fargs);
}
#endif

static emacs_value sqlite3_api_open(
    emacs_env *env,
    ptrdiff_t n,
    emacs_value *args,
    void *ptr) {
  (void)ptr;
  (void)n;

  // Filename
  char *db_file = 0;
  if (extract_string_arg(env, args[0], &db_file)) {
    return SYM(env, "nil");
  }

  // FLAGS
  int flags = 0;
  for (int i = 1; i < n; i++) {
    flags |= env->extract_integer(env, args[i]);
    NON_LOCAL_EXIT_CHECK(env);
  }

  sqlite3 *dbh = 0;
  int rv = sqlite3_open_v2(db_file, &dbh, flags, 0);
  INFO(env, "%s: file=%s, flags=%d, rv=%d", __func__, db_file, flags, rv);
  FREE(db_file);

  if (rv != SQLITE_OK) {
    if (dbh)
      sqlite3_free(dbh);
    signal_error(env, "db-error", "sqlite_open_v2() failed", rv);
    return SYM(env, "nil");
  }

  return env->make_user_ptr(env, sqlite3_dbh_gc, dbh);
}

/* (define-error err_sym err_desc) */
static void define_error(
    emacs_env *env,
    const char *err_sym,
    const char *err_desc) {
  emacs_value argv[] = {
    SYM(env, err_sym),
    env->make_string(env, err_desc, strlen(err_desc))
  };
  env->funcall(env, SYM(env, "define-error"), 2, argv);
}

/* Since defconst is a special form, args are NOT evaluated. Hence
   eval is needed:

   (eval (list '(defconst sym val)) t)

   Reference: https://phst.github.io/emacs-modules#funcall
 */
static void defconst(emacs_env *env, const char *sym, emacs_value val) {
  emacs_value list_argv[] = {
    SYM(env, "defconst"),
    SYM(env, sym),
    val
  };
  emacs_value form = make_list(env, 3, list_argv);
  emacs_value eval_argv[] = { form, SYM(env, "t") };
  env->funcall(env, SYM(env, "eval"), 2, eval_argv);
}

int emacs_module_init(struct emacs_runtime *ert) {
    emacs_env *env = ert->get_environment(ert);

    struct lisp_func {
      const char *lisp_func_name;
      ptrdiff_t min_arity;
      ptrdiff_t max_arity;
      emacs_value (*function) (emacs_env *env,
                               ptrdiff_t nargs,
                               emacs_value args[],
                               void *) EMACS_NOEXCEPT;
      const char *documentation;
    };

    struct lisp_func all_funcs[] = {
      { "sqlite3-open", 1, 10, sqlite3_api_open,
        "Open a SQLite3 database." },
      { "sqlite3-close", 1, 1, sqlite3_api_close,
        "Close a SQLite3 database." },
      { "sqlite3-prepare", 2, 2, sqlite3_api_prepare,
        "Prepare (compile) a SQL statement." },
      { "sqlite3-finalize", 1, 127, sqlite3_api_finalize,
        "Destroy a prepared statement." },
      { "sqlite3-changes", 1, 1, sqlite3_api_changes,
        "Count the number of rows modified." },
      { "sqlite3-step", 1, 1, sqlite3_api_step,
        "Evaluate a SQL statement." },
      { "sqlite3-reset", 1, 1, sqlite3_api_reset,
        "Reset a prepared SQL statement." },
      { "sqlite3-last-insert-rowid", 1, 1, sqlite3_api_last_insert_rowid,
        "Return last insert rowid." },
#if 0
      { "sqlite3-set-last-insert-rowid", 2, 2,
        sqlite3_api_set_last_insert_rowid,
        "Set last insert rowid." },
#endif
      { "sqlite3-get-autocommit", 1, 1, sqlite3_api_get_autocommit,
        "Test for auto-commit mode." },
      { "sqlite3-exec", 2, 3, sqlite3_api_exec,
        "One-step query execution interface." },
      { "sqlite3-set-log-level", 1, 1, sqlite3_api_set_log_level,
        "Set log level (DEBUG 0, INFO 1, WARN 2, ERROR 3, NOLOG 100)." },

      /* bind interface */
      { "sqlite3-bind-text", 3, 3, sqlite3_api_bind_text,
        "Bind text to a prepared SQL statement." },
      { "sqlite3-bind-int64", 3, 3, sqlite3_api_bind_int64,
        "Bind int64 to a prepared SQL statement." },
      { "sqlite3-bind-double", 3, 3, sqlite3_api_bind_double,
        "Bind double to a prepared SQL statement." },
      { "sqlite3-bind-null", 2, 2, sqlite3_api_bind_null,
        "Bind NULL to a prepared SQL statement." },
      { "sqlite3-bind-parameter-count", 1, 1,
        sqlite3_api_bind_parameter_count,
        "Return the number of SQL parameters." },
      { "sqlite3-bind-multi", 1, 127, sqlite3_api_bind_multi,
        "Bind multiple parameters to a prepared SQL statement." },

      /* Result */
      { "sqlite3-column-count", 1, 1, sqlite3_api_column_count,
        "Return the number of rows in a result set." },
      { "sqlite3-column-name", 2, 2, sqlite3_api_column_name,
        "Return the name of a column." },
      { "sqlite3-column-type", 2, 2, sqlite3_api_column_type,
        "Return the datatype of a column." },
      { "sqlite3-column-text", 2, 2, sqlite3_api_column_text,
        "Return text data of a column." },
      { "sqlite3-column-int64", 2, 2, sqlite3_api_column_int64,
        "Return int64 data of a column." },
      { "sqlite3-column-double", 2, 2, sqlite3_api_column_double,
        "Return double data of a column." },
      { "sqlite3-fetch", 1, 1, sqlite3_api_fetch,
        "Return a row as a list." },

      { NULL, 0, 0, NULL, NULL }
    };

    for (int i = 0; all_funcs[i].lisp_func_name != NULL; i++) {
      bind_func(env,
                all_funcs[i].lisp_func_name,
                all_funcs[i].min_arity,
                all_funcs[i].max_arity,
                all_funcs[i].function,
                all_funcs[i].documentation);
    }
    sqlite3_api_log_level = SQLITE3_LOG_LEVEL_ERROR;

    /* consts.c includes all the (defconst sqlite-xxx ....) function
       calls which is generated by tools/gen-consts.py */
#include "consts.c"

    define_error(env, "db-error", "Database Error");
    define_error(env, "sql-error", "SQL Error");

    /* (provide 'sqlite3-module ) */
    emacs_value provide = SYM(env, "provide");
    emacs_value mod = SYM(env, "sqlite3-api");
    env->funcall(env, provide, 1, &mod);
    return 0;
}
