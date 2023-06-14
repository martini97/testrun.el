;;; testrun-pytest-test.el --- Tests for testrun and pytest -*- lexical-binding: t -*-

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

;; Integration test between testrun and pytest.

;;; Code:

(require 'ert)
(require 'el-mock)

(ert-deftest test-core-testrun--pytest-inside-class ()
  "Test `testrun--pytest-get-test' inside class."
  (with-mock
    (stub buffer-file-name => (expand-file-name "test/assets/test_python_pytest.py" default-directory))
    (stub project-current => (list 'vc 'Git default-directory))
    (testrun-treesit-test (:language python :mode python-ts-mode :content "test_python_pytest.py" :position 215)
      (should (equal (testrun--pytest-get-test "nearest") "test/assets/test_python_pytest.py::TestWithNamespace::test_inside_namespace"))
      (should (equal (testrun--pytest-get-test "namespace") "test/assets/test_python_pytest.py::TestWithNamespace"))
      (should (equal (testrun--pytest-get-test "file") "test/assets/test_python_pytest.py"))
      (should (equal (testrun--pytest-get-test "all") "")))))

(ert-deftest test-core-testrun--pytest-root-level ()
  "Test `testrun--pytest-get-test' at root level test."
  (with-mock
    (stub buffer-file-name => (expand-file-name "test/assets/test_python_pytest.py" default-directory))
    (stub project-current => (list 'vc 'Git default-directory))
    (testrun-treesit-test (:language python :mode python-ts-mode :content "test_python_pytest.py" :position 80)
      (should (equal (testrun--pytest-get-test "nearest") "test/assets/test_python_pytest.py::test_root_level"))
      (should (equal (testrun--pytest-get-test "namespace") "test/assets/test_python_pytest.py"))
      (should (equal (testrun--pytest-get-test "file") "test/assets/test_python_pytest.py"))
      (should (equal (testrun--pytest-get-test "all") "")))))

(ert-deftest test-testrun-pytest ()
  "Test if it calls compile with the expected parameters."
  (with-mock
    (stub buffer-file-name => (expand-file-name "test/assets/test_python_pytest.py" default-directory))
    (stub project-current => (list 'vc 'Git default-directory))
    (mock (compile "pytest test/assets/test_python_pytest.py::TestWithNamespace::test_inside_namespace" t))
    (testrun-treesit-test (:language python :mode python-ts-mode :content "test_python_pytest.py" :position 215)
      (testrun-nearest)))

  (with-mock
    (stub buffer-file-name => (expand-file-name "test/assets/test_python_pytest.py" default-directory))
    (stub project-current => (list 'vc 'Git default-directory))
    (mock (compile "pytest test/assets/test_python_pytest.py::TestWithNamespace" t))
    (testrun-treesit-test (:language python :mode python-ts-mode :content "test_python_pytest.py" :position 215)
      (testrun-namespace)))

  (with-mock
    (stub buffer-file-name => (expand-file-name "test/assets/test_python_pytest.py" default-directory))
    (stub project-current => (list 'vc 'Git default-directory))
    (mock (compile "pytest test/assets/test_python_pytest.py" t))
    (testrun-treesit-test (:language python :mode python-ts-mode :content "test_python_pytest.py" :position 215)
      (testrun-file)))

  (with-mock
    (stub buffer-file-name => (expand-file-name "test/assets/test_python_pytest.py" default-directory))
    (stub project-current => (list 'vc 'Git default-directory))
    (mock (compile "pytest" t))
    (testrun-treesit-test (:language python :mode python-ts-mode :content "test_python_pytest.py" :position 215)
      (testrun-all))))

(provide 'testrun-pytest-test)
;;; testrun-pytest-test.el ends here
