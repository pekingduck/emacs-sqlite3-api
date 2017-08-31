CC = gcc
CFLAGS = -g3 -Wall -std=c99
EMACS252=$(HOME)/test-emacs/bin/emacs
EMACS251=$(HOME)/test-emacs-251/bin/emacs
MODULE=sqlite3-api-module
MODULE_VERSION=0.0.1
MODULE_BASENAME=$(MODULE)-$(MODULE_VERSION)
MODULE_PKG_EL=$(MODULE_BASENAME)/$(MODULE)-pkg.el
MODULE_TAR=$(MODULE_BASENAME).tar

all: $(MODULE).so

clean:
	rm -f *.so *.o *.tar $(MODULE)-pkg.el MODULE

consts:
	(cd tools; ./run.sh)

# File "MODULE" is read by (sqlite3-api-install-dynamic-module)
# during installation
module: $(MODULE).so
	mkdir $(MODULE_BASENAME)
	echo "(define-package \"$(MODULE)\" \"$(MODULE_VERSION)\" \"SQLite3 API dynamic module\")" > $(MODULE_PKG_EL)
	cp $(MODULE).so sqlite3-api-constants.el $(MODULE_BASENAME)
	tar cvf $(MODULE_TAR) $(MODULE_BASENAME)
	echo $(MODULE_TAR) > MODULE

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
