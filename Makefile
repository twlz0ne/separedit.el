EMACS ?= emacs
ELS = separedit.el
ELCS = separedit.elc
TEST_ELS = test/test-bootstrap.el test/separedit-test-helper.el test/separedit-test.el

# If the first argument is "test"...
ifeq (test, $(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  SELECTOR := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(SELECTOR):;@:)
endif

all: compile test readme clean

%.elc:%.el
	$(EMACS) -batch -L . -l test/test-bootstrap.el -f batch-byte-compile $(ELS)

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
	   (separedit-test--generate-readme))\
	  (separedit--remove-comment-delimiter\
	   (separedit--comment-delimiter-regexp 'emacs-lisp-mode))\
	  (write-file filename)))"

help:
	@echo make
	@echo make compile
	@echo make test [SELECTOR]
	@echo make readme
	@echo make clean

clean:
	@rm -f *.elc
