EMACS ?= emacs
CASK ?= cask
TEST_DIR=$(shell pwd)/tests
MATCH ?=

.PHONY: test

test:
	$(CASK) exec ert-runner -l test/test-prelude.el
