EMACS ?= emacs
CASK ?= cask
TEST_DIR=$(shell pwd)/tests
MATCH ?=

for_compile := *.el
for_checkdoc := *.el
for_checkindent := *.el

.PHONY: test clean-elc compile lint

test: clean-elc
	@$(CASK) exec ert-runner -l test/test-prelude.el

clean-elc: ## Delete cached files
	@find . -name '*.elc' -delete
	@find . -name '*~' -delete
	@find . -name '*.el-autoloads.el' -delete

compile: clean-elc ## Check for byte-compiler errors
	@for file in $(for_compile); do \
		echo "[compile] $$file"; \
		$(CASK) emacs -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
			| grep -i -v '^wrote' | grep . && exit 1 || true; \
	done

lint: clean-elc
	@$(CASK) emacs -Q --batch -l scripts/testrun-lint.el -f elisp-lint-files-batch *.el
