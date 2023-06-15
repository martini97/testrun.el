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
(require 'testrun-ert)

(cl-defmacro test-testrun-treesit-setup (&rest args)
  "Setup test buffer for treesiter with ARGS.

:mode is the mode to activate
:language is the treesitter language
:asset is the filename of the asset to load
:position is the point in the file to go to
:body is the test to run in the buffer context, since I suck at
elisp it only supports a `progn'."
  (declare (indent defun) (debug (sexp body)))
  `(with-temp-buffer
     (funcall #',(plist-get args :mode))
     (treesit-parser-create ',(plist-get args :language))
     (switch-to-buffer (current-buffer))
     (erase-buffer)
     (goto-char (point-min))
     (insert-file-contents (expand-file-name ,(plist-get args :asset) "test/assets/"))
     (goto-char ,(plist-get args :position))
     (let ((buffer-file-name (expand-file-name ,(plist-get args :asset) "test/assets/")))
       ,(plist-get args :body))))

(cl-defmacro test-testrun-setup (&rest args)
  "Setup test buffer with ARGS.

:mode is the mode to activate
:asset is the filename of the asset to load
:position is the point in the file to go to
:body is the test body"
  (declare (indent defun) (debug (sexp body)))
  `(cl-letf (((symbol-function 'project-current)
              (lambda () (list 'vc 'Git default-directory))))
     (with-temp-buffer
       (funcall #',(plist-get args :mode))
       (switch-to-buffer (current-buffer))
       (erase-buffer)
       (goto-char (point-min))
       (insert-file-contents (expand-file-name ,(plist-get args :asset) "test/assets/"))
       (goto-char ,(plist-get args :position))
       (let ((buffer-file-name (expand-file-name ,(plist-get args :asset) "test/assets/")))
         ,(plist-get args :body)))))

(provide 'test-prelude)
;;; test-prelude.el ends here

