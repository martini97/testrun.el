;;; testrun-pytest.el --- Pytest wrapper for testrun -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>
;; Mantainer: Alessandro Martini <martini97@protonmail.ch>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29"))
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

;; Pytest helpers for `testrun'.

;;; Code:

(require 'testrun-core)
(require 'testrun-treesit)

(defcustom testrun-pytest-separator "::"
  "Separator for the pyests test path."
  :type 'string
  :group 'testrun)

(defvar testrun-pytest-namespace-node-types '("class_definition")
  "List of node types relevant for the \"namespace\" scope.")

(defvar testrun-pytest-nearest-node-types '("class_definition"
                                            "function_definition")
  "List of node types relevant for the \"nearest\" scope.")

(defun testrun-pytest--get-nodes (node-types)
  "Return name of nodes with NODE-TYPES."
  (mapcar #'testrun-treesit--get-node-name
          (testrun-treesit--get-nodes-by-type node-types)))

;;;###autoload
(defun testrun-pytest-get-test (scope)
  "Get the pytest test specifier string for the SCOPE."
  (string-join
   (let ((filename (testrun-core--file-name)))
     (pcase scope
       ("nearest" (append (list filename)
                          (testrun-pytest--get-nodes testrun-pytest-nearest-node-types)))
       ("namespace" (append (list filename)
                            (testrun-pytest--get-nodes testrun-pytest-namespace-node-types)))
       ("file" (list filename))
       ("all" nil)))
   testrun-pytest-separator))

(provide 'testrun-pytest)
;;; testrun-pytest.el ends here
