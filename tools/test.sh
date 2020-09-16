#!/bin/sh

# run tests with different versions of Emacs
make EMACS="/Applications/Emacs\ 25-3.app/Contents/MacOS/Emacs" test
make EMACS="/Applications/Emacs\ 26-3.app/Contents/MacOS/Emacs" test
make EMACS=/usr/local/bin/emacs test # 27.1
