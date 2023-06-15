;;; testrun-test.el --- Test runner tests -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini

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

(require 'cl-macs)

(ert-deftest test-testrun--get-test ()
  "Tests for `testrun--get-test'."
  (defun mock-test-runner (type)
    (should (equal type "test")))

  (let ((testrun-runner-function-alist '((test . mock-test-runner))))
    (testrun--get-test "test" 'test)

    (should-error (testrun--get-test "test" 'unknown)
                  :type 'user-error)))

(ert-deftest test-testrun--get-runner ()
  "Tests for `testrun--get-runner'."
  (let ((testrun-mode-alist '((python-mode . pytest)
                              ("/project/regex_test\\.py" . regex))))
    (let ((major-mode 'python-mode))
      (should (equal (testrun--get-runner) 'pytest)))

    (let ((buffer-file-name "/project/regex_test.py"))
      (should (equal (testrun--get-runner) 'regex)))))

(ert-deftest test-testrun--get-runner-cmd ()
  "Tests for `testrun--get-runner-cmd'."
  (let ((testrun-runners '((pytest "pytest"))))
    (should (equal (testrun--get-runner-cmd 'pytest) '("pytest")))
    (should-error (testrun--get-runner-cmd 'unknown)
                  :type 'user-error)))

(ert-deftest test-testrun--comint-p ()
  "Tests for `testrun--comint-p'."
  (let ((testrun-comint-runners '(pytest)))
    (should (testrun--comint-p 'pytest))
    (should-not (testrun--comint-p 'unknown))))

(ert-deftest test-testrun--get-test-cmd ()
  "Tests for `testrun--get-test-cmd'."
  (cl-letf (((symbol-function 'testrun--get-runner-cmd)
             (lambda (runner)
               (should (equal runner 'test))
               '("test")))
            ((symbol-function 'testrun--get-test)
             (lambda (scope runner)
               (should (equal runner 'test))
               (should (equal scope "nearest"))
               "cmd args")))
    (should (equal (testrun--get-test-cmd 'test "nearest")
                   "test cmd args"))))

(ert-deftest test-testrun--compile ()
  "Tests for `testrun--compile'."
  ;; TODO: Test it saves remembers the command
  ;; TODO: Test it sets the expected buffer name function (?)
  ;; TODO: Test it calls the compile command properly
  )

(provide 'testrun-test)
;;; testrun-test.el ends here

