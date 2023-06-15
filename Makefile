EMACS ?= emacs
CASK ?= cask
TEST_DIR=$(shell pwd)/tests
MATCH ?=

for_compile := *.el
for_checkdoc := *.el
for_checkindent := *.el

.PHONY: test clean-elc compile lint help

test: clean-elc ## Run tests with ERT
	@$(CASK) exec ert-runner

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

lint: clean-elc ## Run linter
	@$(CASK) emacs -Q --batch -l scripts/testrun-lint.el -f elisp-lint-files-batch *.el

install: ## Install Cask packages
	@$(CASK) install

help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST) | \
		sed 's/^/  make /' | \
		sed 's/:[^#]*[#]# /|/' | \
		sed 's/%/LANG/' | \
		column -t -s'|' >&2
