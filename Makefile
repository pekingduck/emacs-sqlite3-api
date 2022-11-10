CC  ?= gcc
INC ?= -I.
LIB ?= -lsqlite3

ifeq ($(HOMEBREW), 1)
 INC=-I/usr/local/opt/sqlite3/include
 LIB=-L/usr/local/opt/sqlite3/lib -lsqlite3
endif

CFLAGS ?= -g3 -Wall -std=c99 $(INC)

EMACS ?= /Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_14

UNAME_O=$(shell uname -o)
ifeq ($(UNAME_O),Cygwin)
SHLIB_EXTENSION=dll
else ifeq ($(UNAME_O),Msys)
SHLIB_EXTENSION=dll
else
SHLIB_EXTENSION=so
endif

# Melpa package
PKG=sqlite3

# dynamic module package
MODULE=$(PKG)
MODULE_VERSION=0.16
MODULE_BASENAME=$(MODULE)-$(MODULE_VERSION)
MODULE_PKG_EL=$(MODULE_BASENAME)/$(MODULE)-pkg.el
MODULE_TAR=$(MODULE_BASENAME).tar

all: $(MODULE)-api.$(SHLIB_EXTENSION)

clean:
	rm -rf *.$(SHLIB_EXTENSION) *.o *.tar $(MODULE_BASENAME)

# File "MODULE" is read by (sqlite3-api-install-dynamic-module)
# during installation
module: $(MODULE)-api.$(SHLIB_EXTENSION)
	mkdir -p $(MODULE_BASENAME)
	cp $(MODULE).el $(MODULE)-api.$(SHLIB_EXTENSION) $(MODULE_BASENAME)
	echo "(define-package \"$(MODULE)\" \"$(MODULE_VERSION)\" \"SQLite3 API dynamic module\")" > $(MODULE_PKG_EL)
	tar cvf $(MODULE_TAR) $(MODULE_BASENAME)

install: module
	emacsclient -e '(package-install-file "$(MODULE_TAR)")'

%.$(SHLIB_EXTENSION): %.o
	$(CC) -shared -o $@ $< $(LIB)

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $<

# Regression test
test:
	$(EMACS) -batch -Q -L . -l tests/regression.el

test-consts:
	$(EMACS) -batch -Q -L . -l tests/consts.el
