CC = gcc
INC=-I.
LIB=-lsqlite3
CFLAGS=-g3 -Wall -std=c99 $(INC)

EMACS252=$(HOME)/test-emacs/bin/emacs
EMACS251=$(HOME)/test-emacs-251/bin/emacs
SQLITE3_H=$(shell tools/find-sqlite3-h.sh $(INC))

# Melpa package
PKG=sqlite3-api

# dynamic module package
MODULE=$(PKG)
MODULE_VERSION=0.1
MODULE_BASENAME=$(MODULE)-$(MODULE_VERSION)
MODULE_PKG_EL=$(MODULE_BASENAME)/$(MODULE)-pkg.el
MODULE_TAR=$(MODULE_BASENAME).tar

all: $(MODULE).so 

clean:
	rm -rf *.so *.o *.tar $(MODULE_BASENAME)

# File "MODULE" is read by (sqlite3-api-install-dynamic-module)
# during installation
module: $(MODULE).so 
	mkdir -p $(MODULE_BASENAME)
	cp $(MODULE).so $(MODULE_BASENAME)
	echo "(define-package \"$(MODULE)\" \"$(MODULE_VERSION)\" \"SQLite3 API dynamic module\")" > $(MODULE_PKG_EL)
	tar cvf $(MODULE_TAR) $(MODULE_BASENAME)

install: module
	emacsclient -e '(package-install-file "$(MODULE_TAR)")'

#consts.c: $(SQLITE_H)
#	grep "^#define SQLITE" $(SQLITE3_H) | tools/gen-consts.py > $@

%.so: %.o
	$(CC) -shared -o $@ $< $(LIB)

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $<

# Emacs 25.2
test:
	$(EMACS252) -batch -Q -L . -l tests/regression.el

# Emacs 25.1
t251:
	$(EMACS251) -batch -Q -L . -l tests/regression.el
