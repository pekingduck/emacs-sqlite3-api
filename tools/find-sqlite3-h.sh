#!/bin/bash
# locate the path of sqlite3.h
echo '#include <sqlite3.h>' | gcc -x c -H -fsyntax-only - 2>&1 | grep '^\. ' | cut -f2 -d' '
