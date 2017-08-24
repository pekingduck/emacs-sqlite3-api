#!/bin/bash

grep "^#define SQLITE" /usr/include/sqlite3.h | ./gen-consts.py > def.c
gcc -o def def.c -lsqlite3
./def > ../sqlite3-napi.el
