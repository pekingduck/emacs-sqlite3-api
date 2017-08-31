CC = gcc
CFLAGS = -g3 -Wall -std=c99
EMACS252=$(HOME)/test-emacs/bin/emacs
EMACS251=$(HOME)/test-emacs-251/bin/emacs
MODULE=sqlite3-api-module
MODULE_VERSION=0.0.1

all: $(MODULE).so

clean:
	rm -f *.so *.o *.tar $(MODULE)-pkg.el MODULE

consts:
	(cd tools; ./run.sh)

# File "MODULE" is read by (sqlite3-api-install-dynamic-module)
# during installation
package: $(MODULE).so
	echo $(MODULE)-$(MODULE_VERSION).tar > MODULE
	echo "(define-package \"$(MODULE)\" \"$(MODULE_VERSION)\" \"SQLite3 API dynamic module\")" > $(MODULE)-pkg.el
	tar cvf $(MODULE)-$(MODULE_VERSION).tar $(MODULE)-pkg.el $(MODULE).so

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
