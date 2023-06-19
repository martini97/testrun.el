;;; testrun-treesit.el --- Testrun treesit library -*- lexical-binding: t -*-

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

;; Treesit helper library for `testrun'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'treesit)

(defun testrun-treesit--node-parents (node)
  "Return a list of all NODE parents or nil."
  (let ((nodes nil))
    (while (setq node (treesit-node-parent node))
      (push node nodes))
    nodes))

(defun testrun-treesit--filter-nodes-by-type (nodes types)
  "Filter NODES with type in TYPES."
  (seq-filter (lambda (node) (member (treesit-node-type node) types))
              nodes))

(defun testrun-treesit--get-nodes-by-type (node-types)
  "Iterate upwards in the tree and collect the nodes with NODE-TYPES."
  (testrun-treesit--filter-nodes-by-type
   (testrun-treesit--node-parents (treesit-node-at (point))) node-types))

(defun testrun-treesit--get-node-name (node)
  "Get name for NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name") t))

(defun testrun-treesit--get-fn-name (node)
  "Get name for function at NODE."
  (treesit-node-text
   (treesit-node-child node 0) t))

(provide 'testrun-treesit)
;;; testrun-treesit.el ends here
