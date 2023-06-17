;;; testrun-jest.el --- Jest wrapper for testrun -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>
;; Mantainer: Alessandro Martini <martini97@protonmail.ch>
;; Version: 0.0.1
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

;; Jest helpers for `testrun'.

;;; Code:

(require 'treesit)
(require 'subr-x)
(require 'testrun-core)
(require 'testrun-treesit)

(defun testrun-jest--node-test-p (node test-keywords)
  "Verify if NODE is one of Jest's TEST-KEYWORDS."
  (let* ((child (treesit-node-child node 0))
         (text (treesit-node-text child t))
         (keyword (car-safe (split-string text "\\."))))
    (not (null (member keyword test-keywords)))))

(defun testrun-jest--node-name (node)
  "Get Jest's test description for NODE."
  (when-let* ((arguments (treesit-node-child-by-field-name node "arguments"))
              (name-node (treesit-node-child (treesit-node-child arguments 1) 1))
              (name (treesit-node-text name-node t)))
    name))

(defun testrun-jest--escape (str)
  "Escape STR following Jest rules."
  (format "'%s'"
          (thread-last str
                       (string-replace "'" "'\"'\"'")
                       (string-replace "(" "\\(")
                       (string-replace ")" "\\)"))))

(defun testrun-jest--get-test-by-keywords (&rest keywords)
  "Generate the Jest selector arguments filtering by KEYWORDS."
  (when-let* ((nodes (testrun-treesit--get-nodes-by-type "call_expression"))
              (keywords (seq-filter
                         #'(lambda (n) (testrun-jest--node-test-p n keywords))
                         nodes))
              (names (seq-map #'testrun-jest--node-name keywords)))
    (list "-t" (testrun-jest--escape (string-join names " ")))))

;;;###autoload
(defun testrun-jest-get-test (type)
  "Get jest test for TYPE."
  (string-join
   (let ((filename (testrun-core--file-name)))
     (pcase type
       ("nearest" (append (list filename)
                          (testrun-jest--get-test-by-keywords
                           "it" "test" "describe")))
       ("namespace" (append (list filename)
                            (testrun-jest--get-test-by-keywords "describe")))
       ("file" (list filename))
       ("all" nil)))
   " "))

(provide 'testrun-jest)
;;; testrun-jest.el ends here
