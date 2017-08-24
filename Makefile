CC = gcc
CFLAGS = -g3 -Wall
EMACS=$(HOME)/test-emacs/bin/emacs

all: sqlite3-napi-module.so

clean:
	rm -f *.so

%.so: %.o
	$(CC) -shared -o $@ $< -lsqlite3

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $<

test:
	$(EMACS) -batch -Q -l regression.el
