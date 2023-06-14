;;; testrun.el --- Test runner for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>
;; Mantainer: Alessandro Martini <martini97@protonmail.ch>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1") project treesit cl-lib)
;; Keywords: tests

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

(defgroup testrun nil
  "Test runner for Emacs."
  :group 'tools)

(defcustom testrun-runners '((pytest . ("pytest"))
                             (jest . (npx "jest")))
  "Alist of test runner commands."
  :type '(alist :key-type (symbol :tag "Runner")
                :value-type (choice (repeat (choice (string :tag "Argument")
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

(provide 'testrun)

;;; testrun.el ends here
