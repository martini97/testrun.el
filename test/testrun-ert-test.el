;;; testrun-ert-test.el --- Tests for testrun and ert -*- lexical-binding: t -*-

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

;; Integration test between testrun and ERT.

;;; Code:

(require 'cl-macs)
(require 'testrun)
(require 'testrun-ert)

(ert-deftest test-testrun-ert-get-test ()
  "Verify the `testrun-ert-get-test' function.

Since the path for ert is simpler because there's no nesting or any
regex shenanigans, all tests are done here."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "ert-test-exaple.el"
    :position 608
    :body
    (progn
      (let* ((filename (expand-file-name "ert-test-exaple.el" "test/assets/"))
             (test-filename (file-relative-name filename default-directory)))
        (should (equal (testrun-ert-get-test "nearest")
                       (concat test-filename " -p '^test-testrun-pytest-get-test-nearest$'")))
        (should-error (testrun-ert-get-test "namespace") :type 'user-error)
        (should (equal (testrun-ert-get-test "file") test-filename))
        (should (equal (testrun-ert-get-test "all") ""))))))

(ert-deftest test-testrun-ert-run ()
  "Sort of an integration test for the compile command of ERT."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "ert-test-exaple.el"
    :position 608
    :body
    (progn
      (let ((test-filename "test/assets/ert-test-exaple.el")
            (test-name "test-testrun-pytest-get-test-nearest")
            (ert-cmd "cask exec ert-runner"))
        (cl-letf
            (((symbol-function 'compile)
              (lambda (cmd commint)
                (should
                 (equal
                  cmd
                  (concat
                   ert-cmd " " test-filename " -p "
                   (testrun-core--get-test-regex test-name t))))
                (should-not commint))))
          (testrun-nearest))))))

(provide 'testrun-ert-test)
;;; testrun-ert-test.el ends here

