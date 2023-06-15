;;; testrun-core.el --- Core library for testrun.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>
;; Mantainer: Alessandro Martini <martini97@protonmail.ch>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1") (project "0.9.8"))
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

;; Core library for `testrun'.

;;; Code:

(require 'project)

(defvar testrun-core--last-tests nil
  "Store previous test run, so we can trigger them again.")

(defun testrun-core--root ()
  "Get root directory for compilation.

This uses `project-current' to find the root directory of
the project and assumes that's the root for the compile command.
If `project-current' cannot find a project, returns the `default-directory'."
  (if-let ((project (project-current)))
      (project-root project)
    default-directory))

(defun testrun-core--file-name ()
  "Get the buffer filename relative to the compilation root."
  (file-relative-name buffer-file-name (testrun-core--root)))

(defun testrun-core--find-node-modules-exe (exe)
  "Find the full path for EXE.

If there's an executable file in the node_modules/.bin with the EXE name
then return the path to that executable, otherwise return the EXE so that
the shell will resolve it with it's $PATH."
  (if-let* ((bin-dir (expand-file-name "node_modules/.bin" (testrun-core--root)))
            (exe-path (expand-file-name exe bin-dir))
            ((file-executable-p exe-path)))
      exe-path
    exe))

(defun testrun-core--resolve-runner-command (runner)
  "Resolve the RUNNER into a list of shell arguments.

This is meant to translate the symbols from the RUNNER into actual commands,
like the `npx' symbol which is translated into the node_modules/.bin."
  (let ((cmd runner))
    (when-let* ((has-npx (memq 'npx runner))
                (exe-name (car-safe (cdr-safe has-npx)))
                (without-npx (remq 'npx runner))
                (exe (testrun-core--find-node-modules-exe exe-name))
                (cmd-full (cl-substitute exe exe-name without-npx)))
      (setq cmd cmd-full))
    cmd))

(defun testrun-core--remember (root cmd with-comint)
  "Remember compilation command for later use.

Store ROOT, CMD and WITH-COMINT in `testrun-core--last-tests'
so we can retrieve them later."
  (assoc-delete-all root testrun-core--last-tests)
  (push `(,root . (,cmd ,with-comint)) testrun-core--last-tests))

(defun testrun-core--get-last (root)
  "Get last known compile command for ROOT.

If there's a command returns the command and the comint flag,
otherwise returns nil."
  (when-let ((known (assoc root testrun-core--last-tests)))
    (cdr known)))

(provide 'testrun-core)
;;; testrun-core.el ends here

