#!/bin/bash
DIR=$(dirname $0)
NOW=$(date '+%Y-%m-%d %H:%M:%S')
URL=https://sqlite.org/c3ref/constlist.html
SQLITE3_H=$(echo '#include <sqlite3.h>' | gcc $* -x c -H -fsyntax-only - 2>&1 | grep '^\. ' | cut -f2 -d' ')
MASTER=master.txt

cat<<EOF
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
  Auto-generated $NOW
  Based on $URL
*/

EOF

curl -s $URL | pandoc -t html -t plain | grep "^SQLITE_" | grep -v SCANSTAT | grep -v SQLITE_STATIC | grep -v SQLITE_TRANSIENT > $DIR/$MASTER
grep '^#define SQLITE_' $SQLITE3_H | $DIR/gen-consts.py $DIR/$MASTER
rm -f $DIR/$MASTER
