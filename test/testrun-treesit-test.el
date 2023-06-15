;;; testrun-treesit-test.el --- Tests for testrun treesit library -*- lexical-binding: t -*-

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

(require 'cl-macs)

(ert-deftest test-testrun-treesit--get-nodes-by-type ()
  "Test for `testrun-treesit--get-nodes-by-type'."
  (test-testrun-treesit-setup
   :mode python-ts-mode
   :language python
   :asset "test_python_pytest.py"
   :position 72
   :body
   (progn
     (let ((nodes (testrun-treesit--get-nodes-by-type "function_definition")))
       (should (length= nodes 1))
       (should (equal (treesit-node-type (car nodes)) "function_definition")))
     (goto-char 204)
     (let ((nodes (testrun-treesit--get-nodes-by-type "function_definition" "class_definition")))
       (should (length= nodes 2))
       (should (equal (treesit-node-type (nth 0 nodes)) "class_definition"))
       (should (equal (treesit-node-type (nth 1 nodes)) "function_definition")))))

  (test-testrun-treesit-setup
   :mode js-ts-mode
   :language javascript
   :asset "javascriptJest.test.js"
   :position 778
   :body
   (progn
     (let ((nodes (testrun-treesit--get-nodes-by-type "call_expression")))
       (should (length= nodes 4))))))

(ert-deftest test-testrun-treesit--get-node-name ()
  "Test for `testrun-treesit--get-node-name'."
  (test-testrun-treesit-setup
   :mode python-ts-mode
   :language python
   :asset "test_python_pytest.py"
   :position 0
   :body
   (progn
     (should (equal (testrun-treesit--get-node-name (treesit-node-on 132 236))
                    "test_inside_namespace"))
     (should (equal (testrun-treesit--get-node-name (treesit-node-on 102 236))
                    "TestWithNamespace"))
     (should (equal (testrun-treesit--get-node-name (treesit-node-on 17 99))
                    "test_root_level")))))

(provide 'testrun-treesit-test)
;;; testrun-treesit-test.el ends here
