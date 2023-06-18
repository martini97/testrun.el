# testrun.el

[![ci](https://github.com/martini97/testrun.el/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/martini97/testrun.el/actions/workflows/test.yml)

An Emacs wrapper for running tests on different granularities, inspired by [vim-test](https://github.com/vim-test/vim-test).

This package uses treesitter for finding the path to the current test and/or namespace, so you will need Emacs 29+ with treesitter support, [masteringemacs has a great post on setting up treesitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter).

## Supported testrunners

Testrun.el allows you to run tests using Emacs `compile` feature, hanlding each test runner abstraction. Currently the following test runners are supported:

| Language           | Test Runner             | Identifier |
| ------------------ | ----------------------- | ---------- |
| Python             | Pytest                  | `pytest`   |
| Javascript (js,ts) | Jest, React Testscripts | `jest`     |
| Emacs Lisp         | ERT                     | `ert`      |

## Available commands

- `testrun-nearest`: run the nearest test at point if there's one

- `testrun-namespace`: run the tests in the namespace (class for python, describe for jest)

- `testrun-file`: run all tests in the file

- `testrun-all`: run all tests in the suite

- `testrun-last`: run the last known test

## Installation

This package is not on any of the Emacs archives, but it can be installed with elpaca with:

``` elisp
(use-package testrun
  :elpaca (:host github :repo "martini97/testrun.el" :files ("testrun.el" "testrun-*.el"))
  :preface
  ;; this will allow you to override the runners on your .dir-locals.el
  (put 'testrun-runners 'safe-local-variable #'listp)

  (global-set-key
   (kbd "C-c t")
   (define-keymap
     :prefix 'my/tests-key-map
     "t" 'testrun-nearest
     "c" 'testrun-namespace
     "f" 'testrun-file
     "a" 'testrun-all
     "l" 'testrun-last)))

```
