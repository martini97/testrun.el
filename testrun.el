;;; testrun.el --- Test runner -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>
;; Mantainer: Alessandro Martini <martini97@protonmail.ch>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1") (project "0.9.8") (s "1.13.0"))
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
(require 's)
(require 'project)
(require 'treesit)

(defgroup testrun nil
  "Test runner for Emacs."
  :group 'tools)

(defcustom testrun-runners '((pytest . ("pytest"))
                             (jest . (npx "jest")))
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
                                (tsx-ts-mode . jest))
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

(defcustom testrun-pytest-separator "::"
  "Separator for the pyests test path."
  :type 'string
  :group 'testrun)

(defvar testrun-runner-function-alist
  '((pytest . testrun-pytest-get-test)
    (jest . testrun-jest-get-test)))

(defun testrun--get-test (type runner)
  "Get test path for TYPE and RUNNER."
  (if-let ((test-fn (alist-get runner testrun-runner-function-alist)))
      (funcall test-fn type)
    (user-error "Unknown runner \"%s\"" runner)))

(defun testrun--project-root ()
  "Get project root to launch compilation."
  (if-let ((project (project-current)))
      (project-root project)
    default-directory))

(defun testrun--file-name ()
  "Get buffer filename relative to the project root."
  (file-relative-name (buffer-file-name) (testrun--project-root)))

(defun testrun--resolve-command (runner)
  "Resolve the command for RUNNER."
  (let ((cmd runner))
    (when-let* ((has-npx (memq 'npx runner))
                (exe-name (car-safe (cdr-safe has-npx)))
                (without-npx (remq 'npx runner))
                ;; TODO: we can improve this `node-exe'
                (node-exe (expand-file-name exe-name "node_modules/.bin"))
                (cmd-full (cl-substitute node-exe exe-name without-npx)))
      (setq cmd (if (file-executable-p (car-safe cmd-full)) cmd-full
                  without-npx)))
    cmd))

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
      (testrun--resolve-command cmd)
    (user-error "Could not find command for runner \"%s\"" runner)))

(defun testrun--comint-p (runner)
  "Check if RUNNER requires `comint-mode'."
  (not (null (member runner testrun-comint-runners))))


;;;###autoload
(defun testrun-run (type)
  "Run test for TYPE."
  (interactive (list (completing-read "Type: " '("nearest" "namespace" "file" "all"))))
  (let* ((runner (testrun--get-runner))
         (runner-cmd (testrun--get-runner-cmd runner))
         (test (testrun--get-test type runner))
         (with-comint (testrun--comint-p runner))
         (compilation-buffer-name-function testrun-compilation-buffer-name-function)
         (cmd (string-trim (s-join " " (append runner-cmd (list test))))))
    (setq-local compile-command cmd)
    (compile cmd with-comint)))

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

(provide 'testrun)

;;; testrun.el ends here
