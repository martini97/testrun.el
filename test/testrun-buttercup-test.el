;;; testrun-buttercup-test.el --- Test for buttercup -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'testrun)
(require 'testrun-buttercup)

(ert-deftest test-testrun-buttercup--get-test-pattern ()
  "Tests for `testrun-buttercup--get-test-pattern'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (progn
      (should (equal (testrun-buttercup--get-test-pattern 'nearest)
                     (concat "'^blackjack--deal-new-hand "
                             "with a deck of jacks "
                             "player has 20, shows hand actions$'")))
      (should (equal (testrun-buttercup--get-test-pattern 'namespace)
                     (concat "'blackjack--deal-new-hand "
                             "with a deck of jacks'"))))))

(ert-deftest test-testrun-buttercup-get-test ()
  "Tests for `testrun-buttercup-get-test'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (progn
      (should-error (testrun-buttercup-get-test "file")
                    :type 'user-error)
      (should (equal (testrun-buttercup-get-test "nearest")
                     (concat "--pattern '^"
                             "blackjack--deal-new-hand "
                             "with a deck of jacks "
                             "player has 20, shows hand actions"
                             "$'")))
      (should (equal (testrun-buttercup-get-test "namespace")
                     (concat "--pattern "
                             "'blackjack--deal-new-hand "
                             "with a deck of jacks'")))
      (should-not (testrun-buttercup-get-test "all")))))

(ert-deftest test-testrun-buttercup-run-emacs ()
  "Sort of an integration test for the compile command of buttercup."
  (let ((testrun-mode-alist '((emacs-lisp-mode . buttercup-emacs))))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd (concat "emacs --batch --funcall buttercup-run-discover "
                                            "--pattern '^blackjack--deal-new-hand with a "
                                            "deck of jacks player has 20, shows hand actions$'")))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-nearest)))

    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd (concat "emacs --batch --funcall buttercup-run-discover "
                                            "--pattern 'blackjack--deal-new-hand with a "
                                            "deck of jacks'")))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-namespace)))

    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd "emacs --batch --funcall buttercup-run-discover"))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-all)))

    (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (should-error (testrun-file) :type 'user-error))))

(ert-deftest test-testrun-buttercup-run-cask ()
  "Sort of an integration test for the compile command of buttercup."
  (let ((testrun-mode-alist '((emacs-lisp-mode . buttercup-cask))))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd (concat "cask exec buttercup --pattern '^blackjack--deal-"
                                            "new-hand with a deck of jacks player has 20, shows "
                                            "hand actions$'")))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-nearest)))

    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd (concat "cask exec buttercup "
                                            "--pattern 'blackjack--deal-new-hand with a "
                                            "deck of jacks'")))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-namespace)))

    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd "cask exec buttercup"))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-all)))

    (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (should-error (testrun-file) :type 'user-error))))

(ert-deftest test-testrun-buttercup-run-eldev ()
  "Sort of an integration test for the compile command of buttercup."
  (let ((testrun-mode-alist '((emacs-lisp-mode . buttercup-eldev))))
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd (concat "eldev test -- --pattern '^blackjack--deal-"
                                            "new-hand with a deck of jacks player has 20, shows "
                                            "hand actions$'")))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-nearest)))

    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd (concat "eldev test -- "
                                            "--pattern 'blackjack--deal-new-hand with a "
                                            "deck of jacks'")))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-namespace)))

    (cl-letf (((symbol-function 'compile)
               (lambda (cmd commint)
                 (should (equal cmd "eldev test --"))
                 (should-not commint))))
      (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (testrun-all)))

    (test-testrun-setup
        :mode emacs-lisp-mode
        :asset "buttercup-example.el"
        :position 7023
        :body
        (should-error (testrun-file) :type 'user-error))))

(provide 'testrun-buttercup-test)
;;; testrun-buttercup-test.el ends here
