CC = gcc
CFLAGS = -g3 -Wall -std=c99
EMACS252=$(HOME)/test-emacs/bin/emacs
EMACS251=$(HOME)/test-emacs-251/bin/emacs
MODULE=sqlite3-api-module.so

all: $(MODULE) misc

clean:
	rm -f *.so *.o

misc:
	(cd tools; ./run.sh)

%.so: %.o
	$(CC) -shared -o $@ $< -lsqlite3

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $<

# Emacs 25.2
test: $(MODULE)
	$(EMACS252) -batch -Q -l regression.el

# Emacs 25.1
test251: $(MODULE)
	$(EMACS251) -batch -Q -l regression.el
