;;; testrun-ert.el --- ERT wrapper for testrun -*- lexical-binding: t -*-

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

;; ERT helpers for `testrun'.

;;; Code:

(require 'testrun-core)

(defun testrun-ert--get-test-name-at-point ()
  "Get the name of the current test at point.

If the point is not within a test, than throw user error.
The logic of this function was borrowed from
https://github.com/tonini/overseer.el"
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (if-let* ((fn-raw (read (current-buffer)))
              (fn-name (car fn-raw))
              ((string= "ert-deftest" fn-name)))
        (symbol-name (cadr fn-raw))
      (user-error "No test at point"))))

;;;###autoload
(defun testrun-ert-get-test (scope)
  "Get the ERT test specifier string for the SCOPE."
  (string-join
   (let ((filename (testrun-core--file-name)))
     (pcase scope
       ("nearest" (list filename "-p" (testrun-ert--get-test-name-at-point)))
       ("namespace" (user-error "ERT does not support the \"%s\" scope" scope))
       ("file" (list filename))
       ("all" nil)))
   " "))

(provide 'testrun-ert)
;;; testrun-ert.el ends here
