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

(require 'cl-macs)

(ert-deftest test-testrun-core--root ()
  "This function should return the compilation root or `default-directory'."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git "/project/root"))))
    (should (equal (testrun-core--root) "/project/root")))
  (let ((default-directory "/default/directory"))
    (cl-letf (((symbol-function 'project-current)
               (lambda () nil)))
      (should (equal (testrun-core--root) default-directory)))))

(ert-deftest test-testrun-core--file-name ()
  "This function should return the current variable `buffer-file-name' relative to the compilation root."
  (let ((buffer-file-name "/testrun/buffer/file/name.el")
        (expected-file-name "file/name.el"))
    (cl-letf (((symbol-function 'project-current)
               (lambda () (list 'vc 'Git "/testrun/buffer"))))
      (should (equal (testrun-core--file-name) expected-file-name)))
    (cl-letf (((symbol-function 'project-current)
               (lambda () nil)))
      (let ((default-directory "/testrun/buffer"))
        (should (equal (testrun-core--file-name) expected-file-name))))))

(ert-deftest test-testrun-core--find-node-modules-exe ()
  "Should return the path for the exe binary.

If the binary exists inside the node modules bin directory and
is executable than it should return it, otherwise it should return
the the binary as is."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git "/testrun/buffer")))
            ((symbol-function 'file-executable-p)
             (lambda (file)
               ;; it should call `file-executable-p' with the full path
               ;; to the binary in node modules.
               (should (equal file "/testrun/buffer/node_modules/.bin/exe"))
               t)))
    (should (equal (testrun-core--find-node-modules-exe "exe")
                   "/testrun/buffer/node_modules/.bin/exe")))

  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git "/testrun/buffer")))
            ((symbol-function 'file-executable-p)
             (lambda (file) nil)))
    (should (equal (testrun-core--find-node-modules-exe "exe")
                   "exe"))))

(ert-deftest test-testrun-core--resolve-runner-command ()
  "Should resolve any know symbols in the runner command.

Currently there is only one known symbol which is `npx', and
it should resolve either to the binary in the node modules bin dir
or to the executable itself, ie:
'(npx \"jest\") => \"node_modules/.bin/jest\" is it exists
'(npx \"jest\") => \"jest\" if it doesn't."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git "/testrun/buffer")))
            ((symbol-function 'file-executable-p)
             (lambda (_f) t)))
    (should (equal (testrun-core--resolve-runner-command '("jest"))
                   '("jest")))
    (should (equal (testrun-core--resolve-runner-command '(npx "jest"))
                   '("/testrun/buffer/node_modules/.bin/jest")))
    (should (equal (testrun-core--resolve-runner-command '(npx "jest" "args"))
                   '("/testrun/buffer/node_modules/.bin/jest" "args")))
    (should (equal (testrun-core--resolve-runner-command '("CI=TRUE" npx "jest" "args"))
                   '("CI=TRUE" "/testrun/buffer/node_modules/.bin/jest" "args"))))
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git "/testrun/buffer")))
            ((symbol-function 'file-executable-p)
             (lambda (_f) nil)))
    (should (equal (testrun-core--resolve-runner-command '("jest"))
                   '("jest")))
    (should (equal (testrun-core--resolve-runner-command '(npx "jest"))
                   '("jest")))
    (should (equal (testrun-core--resolve-runner-command '(npx "jest" "args"))
                   '("jest" "args")))
    (should (equal (testrun-core--resolve-runner-command '("CI=TRUE" npx "jest" "args"))
                   '("CI=TRUE" "jest" "args")))))

(provide 'testrun-core-test)
;;; testrun-core-test.el ends here
