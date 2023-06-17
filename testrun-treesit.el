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

(require 'treesit)

(defun testrun-treesit--get-nodes-by-type (&rest node-types)
  "Iterate upwards in the tree and collect the nodes with NODE-TYPES."
  (let ((node (treesit-node-at (point)))
        (nodes nil))
    (while node
      (when-let ((node-type (treesit-node-type node))
                 ((member node-type node-types)))
        (push node nodes))
      (setq node (treesit-node-parent node)))
    nodes))

(defun testrun-treesit--get-node-name (node)
  "Get name for NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name") t))

(provide 'testrun-treesit)
;;; testrun-treesit.el ends here
