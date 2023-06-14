;;; test-prelude.el --- Prelude for testrun tests -*- lexical-binding: t -*-

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

;;; Code:

(setq load-prefer-newer t
      load-path (append (list default-directory) load-path)
      python-indent-guess-indent-offset nil)

(require 'cl-lib)
(require 'testrun)
(require 'testrun-treesit)
(require 'testrun-pytest)
(require 'testrun-jest)

(cl-defmacro testrun-treesit-test ((&key language mode content position) &rest body)
  "Helper to open test/test-prelude.elan asset on a buffer.

BODY is the actual tests that will be run."
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (funcall #',mode)
     (treesit-parser-create ',language)
     (switch-to-buffer (current-buffer))
     (erase-buffer)
     (goto-char (point-min))
     (insert-file (expand-file-name ,content "test/assets/"))
     (goto-char ,position)
     ,@body))

(provide 'test-prelude)
;;; test-prelude.el ends here

