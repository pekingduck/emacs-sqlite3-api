EMACS281=$(HOME)/Emacs/dists/28.1/bin/emacs
EMACS272=$(HOME)/Emacs/dists/27.2/bin/emacs

test:
	$(EMACS281) -batch -Q -L .. -l regression.el
	$(EMACS272) -batch -Q -L .. -l regression.el

#test-consts:
#	$(EMACS) -l tests/consts.el
