CC = gcc
INC=-I.
LIB=-lsqlite3

ifeq ($(HOMEBREW), 1)
 INC=-I/usr/local/opt/sqlite3/include
 LIB=-L/usr/local/opt/sqlite3/lib -lsqlite3
endif

CFLAGS=-g3 -Wall -std=c99 $(INC)

EMACS252=$(HOME)/test-emacs/bin/emacs
EMACS251=$(HOME)/test-emacs-251/bin/emacs
EMACS253=$(HOME)/test-emacs-253/bin/emacs
EMACS260=$(HOME)/test-emacs-260/bin/emacs --module-assertions

# Melpa package
PKG=sqlite3-api

# dynamic module package
MODULE=$(PKG)
MODULE_VERSION=0.11
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

%.so: %.o
	$(CC) -shared -o $@ $< $(LIB)

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $<

# Emacs 25.3
test:
	$(EMACS253) -batch -Q -L . -l tests/regression.el

t252:
	$(EMACS252) -batch -Q -L . -l tests/regression.el

t251:
	$(EMACS251) -batch -Q -L . -l tests/regression.el

t260:
	$(EMACS260) -batch -Q -L . -l tests/regression.el	
