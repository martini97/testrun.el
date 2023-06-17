;;; testrun.el --- Test runner -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>
;; Mantainer: Alessandro Martini <martini97@protonmail.ch>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29") (project "0.9.8"))
;; Keywords: tests convenience
;; Homepage: https://github.com/martini97/testrun.el

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

;; Testrun.el is an Emacs package which allows you to run tests using the
;; compile command.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'treesit)

(require 'testrun-core)

(defgroup testrun nil
  "Test runner for Emacs."
  :group 'tools)

(defcustom testrun-runners '((pytest . ("pytest"))
                             (jest . (npx "jest"))
                             (ert . ("cask" "exec" "ert-runner")))
  "Alist of test runner commands."
  :type '(alist :key-type (symbol :tag "Runner")
                :value-type
                (choice (repeat (choice (string :tag "Argument")
                                        (const :tag "Look for command in node_modules/.bin" npx)))))
  :group 'testrun)

(defcustom testrun-mode-alist '((python-mode . pytest)
                                (python-ts-mode . pytest)
                                (js-mode . jest)
                                (js-ts-mode . jest)
                                (typescript-mode . jest)
                                (typescript-ts-mode . jest)
                                (tsx-ts-mode . jest)
                                (emacs-lisp-mode . ert))
  "Alist mapping major mode names to runners to use in thos modes."
  :type '(alist :key-type (choice (symbol :tag "Major mode")
                                  (string :tag "Buffer name regexp"))
                :value-type (symbol :tag "Formatter"))
  :group 'testrun)

(defcustom testrun-comint-runners '(pytest jest)
  "List of runners which must be run with `comint-mode'."
  :type '(list :value-type symbol)
  :group 'testrun)

(defcustom testrun-compilation-buffer-name-function 'project-prefixed-buffer-name
  "Function to compute the name of the test compilation buffer."
  :type 'function
  :group 'testrun)

(defvar testrun-runner-function-alist
  '((pytest . testrun-pytest-get-test)
    (jest . testrun-jest-get-test)
    (ert . testrun-ert-get-test)))

(defun testrun--get-test (type runner)
  "Get test path for TYPE and RUNNER."
  (if-let ((test-fn (alist-get runner testrun-runner-function-alist)))
      (funcall test-fn type)
    (user-error "Unknown runner \"%s\"" runner)))

(defun testrun--get-runner ()
  "Get runner for the current buffer."
  (cl-dolist (runner testrun-mode-alist)
    (when (or (and (symbolp (car runner))
                   (derived-mode-p (car runner)))
              (and (stringp (car runner))
                   buffer-file-name
                   (string-match-p
                    (car runner) buffer-file-name)))
      (cl-return (cdr runner)))))

(defun testrun--get-runner-cmd (runner)
  "Get base command for RUNNER."
  (if-let* ((cmd (alist-get runner testrun-runners)))
      (testrun-core--resolve-runner-command cmd)
    (user-error "Could not find command for runner \"%s\"" runner)))

(defun testrun--comint-p (runner)
  "Check if RUNNER requires `commint-mode'."
  (not (null (member runner testrun-comint-runners))))

(defun testrun--get-test-cmd (runner scope)
  "Get test command for compile.

RUNNER is the runner symbol, SCOPE is the test scope."
  (let* ((runner-cmd (testrun--get-runner-cmd runner))
         (test (testrun--get-test scope runner)))
    (string-trim (string-join (append runner-cmd (list test)) " "))))

(defun testrun--compile (command with-comint root)
  "Compile COMMAND WITH-COMINT at ROOT."
  (testrun-core--remember root command with-comint)
  (let ((compilation-buffer-name-function testrun-compilation-buffer-name-function)
        (default-directory root))
    (compile command with-comint)))

;;;###autoload
(defun testrun-run (scope)
  "Run test for SCOPE."
  (interactive (list (completing-read "Scope: " '("nearest" "namespace" "file" "all"))))
  (let* ((runner (testrun--get-runner))
         (cmd (testrun--get-test-cmd runner scope))
         (root (testrun-core--root))
         (with-comint (testrun--comint-p runner)))
    (testrun--compile cmd with-comint root)))

;;;###autoload
(defun testrun-nearest ()
  "Shortcut to run the nearest test."
  (interactive)
  (testrun-run "nearest"))

;;;###autoload
(defun testrun-namespace ()
  "Shortcut to run every test in namespace."
  (interactive)
  (testrun-run "namespace"))

;;;###autoload
(defun testrun-file ()
  "Shortcut to run every test in file."
  (interactive)
  (testrun-run "file"))

;;;###autoload
(defun testrun-all ()
  "Shortcut to run every test in the suite."
  (interactive)
  (testrun-run "all"))

;;;###autoload
(defun testrun-last ()
  "Shortcut to run the last test."
  (interactive)
  (if-let* ((root (testrun-core--root))
            (last-known (testrun-core--get-last root)))
      (let ((cmd (car last-known))
            (with-comint (nth 1 last-known)))
        (testrun--compile cmd with-comint root))
    (user-error "No last known test")))

(provide 'testrun)

;;; testrun.el ends here
