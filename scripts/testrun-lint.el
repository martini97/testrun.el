;;; testrun-lint.el -- Setup linting rules -*- lexical-binding: t -*-

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

;; This will setup the linting rules for the Make lint task, I havent
;; found a proper way to make the batch command load the .dir-locals.el
;; values, which would make this obsolete.

;;; Code:


(require 'elisp-lint)

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil
              fill-column 100)

;; skipping this check since we depend on a version
;; higher than the current emacs head
(fset #'package-lint--check-emacs-version #'ignore)

(provide 'testrun-lint)
;;; testrun-lint.el ends here

