# testrun.el

[![ci](https://github.com/martini97/testrun.el/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/martini97/testrun.el/actions/workflows/test.yml)

generic test runner for Emacs, heavily inspired by [vim-test](https://github.com/vim-test/vim-test). provides:

- `testrun-nearest`: run the nearest test at point if there's one

- `testrun-namespace`: run the tests in the namespace (class for python, describe for jest)

- `testrun-file`: run all tests in the file

- `testrun-all`: run all tests in the suite

- `testrun-last`: run the last known test

this package use treesitter for figuring out the test and namespace at point, so you will need at least emacs
29 with treesitter builtin. [masteringemacs has a great post on setting up treesitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter).

## supported runners

- [x] pytest (nearest, namespace, file, all)
- [x] jest (nearest, namespace, file, all)
- [x] ert-runner (nearest, file, all)

## installation

this package is not on any of the Emacs archives, but it can be installed with elpaca with:

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
