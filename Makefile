CC = gcc
CFLAGS = -g3 -Wall -std=c99
EMACS252=$(HOME)/test-emacs/bin/emacs
EMACS251=$(HOME)/test-emacs-251/bin/emacs
SQLITE3_H=$(shell tools/find-sqlite3-h.sh)

# Melpa package
PKG=sqlite3-api

# dynamic module package
MODULE=$(PKG)
MODULE_SO=$(MODULE)-module.so
MODULE_VERSION=0.1
MODULE_BASENAME=$(MODULE)-$(MODULE_VERSION)
MODULE_EL=$(MODULE).el
MODULE_PKG_EL=$(MODULE_BASENAME)/$(MODULE)-pkg.el
MODULE_TAR=$(MODULE_BASENAME).tar

all: consts.c $(MODULE_SO)

clean:
	rm -rf *.so *.o *.tar consts.c install.el

# File "MODULE" is read by (sqlite3-api-install-dynamic-module)
# during installation
module: consts.c $(MODULE_SO)
	mkdir -p $(MODULE_BASENAME)
	cp $(MODULE_SO) $(MODULE_EL) $(MODULE_BASENAME)
	echo "(define-package \"$(MODULE)\" \"$(MODULE_VERSION)\" \"SQLite3 API dynamic module\")" > $(MODULE_PKG_EL)
	tar cvf $(MODULE_TAR) $(MODULE_BASENAME)
	echo "(package-install-file \"$(MODULE_TAR)\")" > install.el

#
install: module
	emacsclient -e '(package-install-file "$(MODULE_TAR)")'

consts.c: $(SQLITE_H)
	grep "^#define SQLITE" $(SQLITE3_H) | tools/gen-consts.py > $@

%.so: %.o
	$(CC) -shared -o $@ $< -lsqlite3

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $<

# Emacs 25.2
test:
	$(EMACS252) -batch -Q -l tests/regression.el

# Emacs 25.1
t251:
	$(EMACS251) -batch -Q -l tests/regression.el
