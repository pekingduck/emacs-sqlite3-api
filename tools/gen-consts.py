#!/usr/bin/env python

import sys
import os
import re

c_src="""
#include <stdio.h>
#include <sqlite3.h>

int main(int argc, char *argv[]) {
"""
print(c_src)
for line in sys.stdin.readlines():
  line.rstrip()
  fields = re.split("\s+", line, 3)
  name = re.sub("_", "-", fields[1].lower())
  if len(fields) > 2 and fields[2] != "":
    #print("<{0}>-<{1}>".format(fields[1], fields[2]), file=sys.stderr)
    if fields[2].startswith('"'):
      print('  printf("(defconst {1} \\"%s\\")\\n", {0});'.format(fields[1], name))
    else:
      print('  printf("(defconst {1} %d)\\n", {0});'.format(fields[1], name))
print('  printf("(provide \'sqlite3-api-constants)\\n");')
print("}")
