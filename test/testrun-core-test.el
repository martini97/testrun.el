;;; testrun-core-test.el --- Tests for testrun core -*- lexical-binding: t -*-

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

(require 'ert)
(require 'el-mock)

(require 'testrun)
(require 'testrun-core)

(ert-deftest test-core-testrun--resolve-command ()
  "Tests for `testrun--resolve-command'."
  (should (equal (testrun--resolve-command '("cmd")) '("cmd")))
  (should (equal (testrun--resolve-command '("cmd" "args" "list")) '("cmd" "args" "list")))
  ;; when there's a executable with the given name in the node_modules, use it.
  (with-mock
    (stub file-executable-p => t)
    (stub expand-file-name => "node_modules/.bin/cmd")
    (should (equal (testrun--resolve-command '(npx "cmd" "args"))
                   '("node_modules/.bin/cmd" "args"))))
  ;; when there isn't a executable with the given name, try to use one from $PATH.
  (with-mock
    (stub file-executable-p => nil)
    (should (equal (testrun--resolve-command '(npx "cmd" "args"))
                   '("cmd" "args")))))

(ert-deftest test-core-testrun--get-runner ()
  "Tests for `testrun--get-runner'."
  (let ((testrun-mode-alist '((test-mode . runner))))
    (let ((major-mode 'test-mode))
      (should (equal (testrun--get-runner) 'runner)))

    (let ((major-mode 'unknown-mode))
      (should (null (testrun--get-runner))))))

(ert-deftest test-core-testrun--get-runner-cmd ()
  "Tests for `testrun--get-runner-cmd'."
  (let ((testrun-runners '((pytest . ("pytest" "--disable-warnings"))
                           (jest . (npx "jest"))
                           (jest-ci . ("CI=true" npx "jest")))))
    (with-mock
     (stub file-executable-p => t)
     (stub expand-file-name => "node_modules/.bin/jest")
     (should (equal (testrun--get-runner-cmd 'jest) '("node_modules/.bin/jest")))
     (should (equal (testrun--get-runner-cmd 'jest-ci) '("CI=true" "node_modules/.bin/jest"))))
    (should (equal (testrun--get-runner-cmd 'pytest) '("pytest" "--disable-warnings")))
    (should-error (testrun--get-runner-cmd 'unknown) :type 'user-error)))

(ert-deftest test-core-testrun--project-root ()
  "Tests for `testrun--project-root'."
  (with-mock
    (stub project-current => (list 'vc 'Git "/project/root"))
    (should (equal (testrun--project-root) "/project/root"))
    (stub project-current => nil)
    (should (equal (testrun--project-root) default-directory))))

(ert-deftest test-core-testrun--file-name ()
  "Tests for `testrun--file-name'."
  (with-mock
    (stub project-current => (list 'vc 'Git "/project/root"))
    (stub buffer-file-name => "/project/root/buffer/filename.ext")
    (should (equal (testrun--file-name) "buffer/filename.ext"))))

(provide 'testrun-core-test)
;;; testrun-core-test.el ends here
