emacs ?= emacs

LOAD =  -l dash.el -l mu4e-maildirs-extension.el

# fake mu4e package
MU4E_FIX = --eval "(provide 'mu4e)"
# fake `mu4e~main-buffer-name' (defined in mu4e)
MU4E_FIX += --eval "(defvar mu4e~main-buffer-name \"tests\")"
# lexical-let is defined in cl
MU4E_FIX += --eval "(require 'cl)"

.PHONY: all compile clean

all: test

test:
	cask exec $(emacs) -batch $(MU4E_FIX) $(LOAD) -l mu4e-maildirs-extension-test.el -f ert-run-tests-batch-and-exit

compile:
	cask exec $(emacs) -batch $(MU4E_FIX) $(LOAD) --eval "(byte-compile-file \"mu4e-maildirs-extension.el\")"

clean:
	rm -f *.elc
