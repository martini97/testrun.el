;;; testrun-sexp.el --- Helpers for elisp sexps -*- lexical-binding: t -*-

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

;; Helpers for elisp sexps.

;;; Code:

(require 'seq)
(require 'thingatpt)

(defun testrun-sexp--beginning-of-thing-at-point-p (thing)
  "Check if current point is at the beginning of THING."
  (equal (point) (car (save-excursion
                        (unless (eq thing 'sexp)
                          (forward-char))
                        (bounds-of-thing-at-point thing)))))

(defun testrun-sexp--read-list-at-point ()
  "Get list at the current point and read it."
  (read
   (if (testrun-sexp--beginning-of-thing-at-point-p 'list)
       (save-excursion
         (forward-char)
         (thing-at-point 'list))
     (thing-at-point 'list))))

(defun testrun-sexp--parents ()
  "Return a list with all the parent sexps of the current point.

If point is at the beginning of a list then it will also be included."
  (let ((parents (if (testrun-sexp--beginning-of-thing-at-point-p 'list)
                     (list (testrun-sexp--read-list-at-point))
                   '())))
    (save-excursion
      (condition-case _err
          (while t
            (backward-up-list)
            (push (testrun-sexp--read-list-at-point) parents))
        (scan-error nil)))
    parents))

(defun testrun-sexp--filter-car-memq (lists wanted)
  "Filter all LISTS where list car is a member of WANTED."
  (seq-filter (lambda (l) (memq (car l) wanted)) lists))

(provide 'testrun-sexp)
;;; testrun-sexp.el ends here
