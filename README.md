# testrun.el

generic test runner for Emacs, heavily inspired by [vim-test](https://github.com/vim-test/vim-test).

## supported runners

- [x] pytest

- [x] jest

## installation

this package is not on any of the Emacs archives, but it can be installed with elpaca with:

``` elisp
(use-package testrun
  :elpaca (:host github
           :repo "martini97/testrun.el"
           :files ("testrun.el" "testrun-*.el")))
```
