# testrun.el

generic test runner for Emacs, heavily inspired by [vim-test](https://github.com/vim-test/vim-test).

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
  :general
  (my-leader-def :infix "t"
    "t" 'testrun-nearest
    "c" 'testrun-namespace
    "f" 'testrun-file
    "a" 'testrun-all))

```
