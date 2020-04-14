#!/bin/bash
# locate the path of sqlite3.h
# Take -I... as the only argument
DIR=$(dirname $0)
NOW=$(date '+%Y-%m-%d %H:%M:%S')
SQLITE3_H=$(echo '#include <sqlite3.h>' | gcc $* -x c -H -fsyntax-only - 2>&1 | grep '^\. ' | cut -f2 -d' ')

cat<<EOF
/*
  Auto-generated $NOW
  Based on $SQLITE3_H
*/

EOF
grep '^#define SQLITE_' $SQLITE3_H | grep -v 'SQLITE_TRANSIENT' | grep -v SQLITE_STATIC | $DIR/gen-consts.py
