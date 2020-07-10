#!/usr/bin/env python3

import sys
import os
import re

useful_codes = []
with open(sys.argv[1]) as f:
  for l in f.readlines():
    useful_codes.append(l.rstrip())

# Read from sqlite3.h (from stdin)
# only codes that exist in useful_codes are included in consts.c
for line in sys.stdin.readlines():
  # fields = [ "#define", "SQLITE_XXXX" "YYYY" ];
  fields = re.split("\s+", line.rstrip(), 3)

  #print("{0}".format(fields[1]))
  if not fields[1] in useful_codes:
    #print("{0} excluded".format(fields[1]))
    continue

  sym = re.sub("_", "-", fields[1].lower())
  if len(fields) > 2 and fields[2] != "":
    print("#ifdef {0}".format(fields[1]))
    if fields[2].startswith('"'):
      print('defconst(env, "{0}", env->make_string(env, {1}, strlen({1})));'.format(sym, fields[1]))
    else:
      print('defconst(env, "{0}", env->make_integer(env, {1}));'.format(sym, fields[1]))
    print("#endif")
