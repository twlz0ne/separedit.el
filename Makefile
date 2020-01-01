EMACS ?= emacs
ELS = comment-edit.el
ELCS = comment-edit.elc
TEST_ELS = cask-bootstrap.el comment-edit-test-helper.el comment-edit-test.el

# If the first argument is "test"...
ifeq (test, $(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  SELECTOR := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(SELECTOR):;@:)
endif

all: compile test readme clean

%.elc:%.el
	$(EMACS) -batch -L . -l cask-bootstrap.el -f batch-byte-compile $(ELS)

compile:$(ELCS)

.PHONY: test readme

test:$(ELCS)
ifeq ($(SELECTOR),)
	$(EMACS) -Q --batch -L . $(addprefix -l , $(TEST_ELS)) -f ert-run-tests-batch-and-exit
else
	$(EMACS) -Q --batch -L . $(addprefix -l , $(TEST_ELS)) --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"
endif

readme:
	$(EMACS) -Q --batch -L . $(addprefix -l , $(TEST_ELS)) --eval "\
	(let ((filename (expand-file-name \"README.md\")))\
	 (with-temp-buffer\
	   (insert\
	    \"<!-- This file was generated from elisp commentary section by tool, DO NOT EDIT -->\n\n\"\
	   (comment-edit-test--generate-readme))\
	  (comment-edit--remove-comment-delimiter\
	   (comment-edit--comment-delimiter-regexp 'emacs-lisp-mode))\
	  (write-file filename)))"

help:
	@echo make
	@echo make compile
	@echo make test [SELECTOR]
	@echo make readme
	@echo make clean

clean:
	@rm -f *.elc
