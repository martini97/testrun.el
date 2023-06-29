;;; testrun-buttercup.el --- Buttercup wrapper for testrun -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>
;; Mantainer: Alessandro Martini <martini97@protonmail.ch>
;; Version: 0.1.1
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

;; Support for Buttercup tests.
;; NOTE: it seems that Buttercup does not offer an option to run all tests in a file,
;; so for now I'm not implementing it.

;;; Code:

(require 'testrun-sexp)
(require 'testrun-core)

(defcustom testrun-buttercup-keywords '((namespace . (describe))
                                        (nearest . (describe it)))
  "Alist of keywords for buttercup tests."
  :type '(alist :key-type (symbol :tag "Scope")
                :value-type (repeat (symbol :tag "Keyword")))
  :group 'testrun)

(defun testrun-buttercup--get-test-pattern (scope)
  "Return the pattern for the test with SCOPE."
  (testrun-core--get-test-regex
   (string-join
    (seq-map #'cadr (testrun-sexp--filter-car-memq
                     (testrun-sexp--parents)
                     (alist-get scope testrun-buttercup-keywords)))
    " ")
   (eq scope 'nearest)))

;;;###autoload
(defun testrun-buttercup-get-test (scope)
  "Get the buttercup test specifier string for the SCOPE."
  (pcase scope
    ("file" (user-error "Buttercup does not support the \"%s\" scope" scope))
    ("nearest" (concat "--pattern " (testrun-buttercup--get-test-pattern 'nearest)))
    ("namespace" (concat "--pattern " (testrun-buttercup--get-test-pattern 'namespace)))
    ("all" nil)))

(provide 'testrun-buttercup)
;;; testrun-buttercup.el ends here
