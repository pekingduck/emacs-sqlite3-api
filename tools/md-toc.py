#!/usr/bin/env python3

# Usage: md-toc.py in_file.md > out_file.md
#
# Generate a TOC for markdown
# for 2nd- ("## ....") and 3rd-level ("### ....") headings
# Put <<TOC>> in the file to mark the location of the TOC

import sys

def main():
  in_file = sys.argv[1]
  toc_lines = []
  lines_before_toc = []
  lines_after_toc = []
  anchor_count = 1

  dest = lines_before_toc

  with open(in_file, 'r') as fp:
    for l in fp.readlines():
      l = l.rstrip()
      if l == '<<TOC>>':
        dest = lines_after_toc
        dest.append('')
      elif l.startswith('## '):
        toc_lines.append(f'* [{l[3:]}](#{anchor_count})')
        dest.append(f'## <a name="{anchor_count}"/> {l[3:]}')
        anchor_count += 1
      elif l.startswith('### '):
        toc_lines.append(f'    * [{l[4:]}](#{anchor_count})')
        dest.append(f'### <a name="{anchor_count}"/> {l[4:]}')
        anchor_count += 1
      else:
        dest.append(l)

  for ls in [lines_before_toc, toc_lines, lines_after_toc]:
    for l in ls:
      print(l)


if __name__ == '__main__':
  main()
