;;; testrun-core.el --- testrun.el core library -*- lexical-binding: t -*-

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

;; `testrun' core library.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'treesit)
(require 'testrun)

(defun testrun--project-root ()
  "Get project root to launch compilation."
  (if-let ((project (project-current)))
      (project-root project)
    default-directory))

(defun testrun--file-name ()
  "Get buffer filename relative to the project root."
  (file-relative-name (buffer-file-name) (testrun--project-root)))

(defun testrun--resolve-command (runner)
  "Resolve the command for RUNNER."
  (let ((cmd runner))
    (when-let* ((has-npx (memq 'npx runner))
                (exe-name (car-safe (cdr-safe has-npx)))
                (without-npx (remq 'npx runner))
                (node-exe (expand-file-name exe-name "node_modules/.bin")) ;; TODO: we can improve this
                (cmd-full (cl-substitute node-exe exe-name without-npx)))
      (setq cmd (if (file-executable-p (car-safe cmd-full)) cmd-full
                  without-npx)))
    cmd))

(defun testrun--get-runner ()
  "Get runner for the current buffer."
  (cl-dolist (runner testrun-mode-alist)
    (when (or (and (symbolp (car runner))
                   (derived-mode-p (car runner)))
              (and (stringp (car runner))
                   buffer-file-name
                   (string-match-p
                    (car runner) buffer-file-name)))
      (cl-return (cdr runner)))))

(defun testrun--get-runner-cmd (runner)
  "Get base command for RUNNER."
  (if-let* ((cmd (alist-get runner testrun-runners)))
      (testrun--resolve-command cmd)
    (user-error "Could not find command for runner \"%s\"" runner)))

(defun testrun--treesit-get-nodes-by-type (&rest node-types)
  "Iterate upwards in the tree and collect the nodes with NODE-TYPES."
  (let ((node (treesit-node-at (point)))
        (nodes nil))
    (while node
      (when-let ((node-type (treesit-node-type node))
                 ((member node-type node-types)))
        (push node nodes))
      (setq node (treesit-node-parent node)))
    nodes))

(defun testrun--treesit-get-node-name (node)
  "Get name for NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name") t))

(defun testrun--pytest-get-test (type)
  "Get pytest path for TYPE."
  (string-join
   (let ((filename (testrun--file-name)))
     (pcase type
       ("nearest" `(,filename . ,(mapcar #'testrun--treesit-get-node-name
                                         (testrun--treesit-get-nodes-by-type
                                          "class_definition" "function_definition"))))
       ("namespace" `(,filename . ,(mapcar #'testrun--treesit-get-node-name
                                           (testrun--treesit-get-nodes-by-type
                                            "class_definition"))))
       ("file" `(,filename))
       ("all" nil)))
   testrun-pytest-separator))

(defun testrun--get-test (type runner)
  "Get test path for TYPE and RUNNER."
  (pcase runner
    ('pytest (testrun--pytest-get-test type))
    (t (user-error "Unknown runner \"%s\"" runner))))

(defun testrun--comint-p (runner)
  "Check if RUNNER requires `comint-mode'."
  (not (null (member runner testrun-comint-runners))))

;;;###autoload
(defun testrun-run (type)
  "Run test for TYPE."
  (interactive (list (completing-read "Type: " '("nearest" "namespace" "file" "all"))))
  (let* ((runner (testrun--get-runner))
         (runner-cmd (testrun--get-runner-cmd runner))
         (test (testrun--get-test type runner))
         (with-comint (testrun--comint-p runner))
         (compilation-buffer-name-function testrun-compilation-buffer-name-function)
         (cmd (string-trim (string-join (append runner-cmd (list test)) " "))))
    (setq-local compile-command cmd)
    (compile cmd with-comint)))

;;;###autoload
(defun testrun-nearest ()
  "Shortcut to run the nearest test."
  (testrun-run "nearest"))

;;;###autoload
(defun testrun-namespace ()
  "Shortcut to run all tests in namespace."
  (testrun-run "namespace"))

;;;###autoload
(defun testrun-file ()
  "Shortcut to run all tests in file."
  (testrun-run "file"))

;;;###autoload
(defun testrun-all ()
  "Shortcut to run all tests."
  (testrun-run "all"))

(provide 'testrun-core)
;;; testrun-core.el ends here
