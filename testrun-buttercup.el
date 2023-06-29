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

(require 'seq)
(require 'thingatpt)
(require 'testrun-core)

(defcustom testrun-buttercup-keywords '((namespace . (describe))
                                        (nearest . (describe it)))
  "Alist of keywords for buttercup tests."
  :type '(alist :key-type (symbol :tag "Scope")
                :value-type (repeat (symbol :tag "Keyword")))
  :group 'testrun)

(defun testrun-buttercup--beginning-of-thing-at-point-p (thing)
  "Predicate to check if current point is at the beginning of THING."
  (equal (point) (car (save-excursion
                        (unless (eq thing 'sexp)
                          (forward-char))
                        (bounds-of-thing-at-point thing)))))

(defun testrun-buttercup--get-list-at-point ()
  "Return the list at point."
  (read
   (if (testrun-buttercup--beginning-of-thing-at-point-p 'list)
       (save-excursion
         (forward-char)
         (thing-at-point 'list))
     (thing-at-point 'list))))

(defun testrun-buttercup--sexp-parents ()
  "Return a list with the all the list parents of the current point.

If point is at the beginning of a list the it will also be included."
  (let ((parents (if (testrun-buttercup--beginning-of-thing-at-point-p 'list)
                     (list (testrun-buttercup--get-list-at-point))
                   '())))
    (save-excursion
      (condition-case _err
          (while t
            (backward-up-list)
            (push (testrun-buttercup--get-list-at-point) parents))
        (scan-error nil)))
    parents))

(defun testrun-buttercup--filter-by-keyword (lists keywords)
  "Filter LISTS that begin with KEYWORDS."
  (seq-filter (lambda (l) (memq (car l) keywords)) lists))

(defun testrun-buttercup--get-test-pattern (scope)
  "Return the pattern for the test with SCOPE."
  (string-join
   (seq-map #'cadr (testrun-buttercup--filter-by-keyword
                    (testrun-buttercup--sexp-parents)
                    (alist-get scope testrun-buttercup-keywords)))
   " "))

;;;###autoload
(defun testrun-buttercup-get-test (scope)
  "Get the buttercup test specifier string for the SCOPE."
  (pcase scope
    ("file" (user-error "Buttercup does not support the \"%s\" scope" scope))
    ("nearest" (concat "--pattern \"^" (testrun-buttercup--get-test-pattern 'nearest) "$\""))
    ("namespace" (concat "--pattern \""(testrun-buttercup--get-test-pattern 'namespace) "\""))
    ("all" nil)))

(provide 'testrun-buttercup)
;;; testrun-buttercup.el ends here
