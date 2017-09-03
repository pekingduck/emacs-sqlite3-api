#!/usr/bin/env python

import sys
import os
import re

for line in sys.stdin.readlines():
  line.rstrip()
  fields = re.split("\s+", line, 3)
  name = re.sub("_", "-", fields[1].lower())
  if len(fields) > 2 and fields[2] != "":
    if fields[2].startswith('"'):
      print('defconst(env, "{0}", env->make_string(env, {1}, strlen({1})));'.format(name, fields[1]))
    else:
      print('defconst(env, "{0}", env->make_integer(env, {1}));'.format(name, fields[1]))
