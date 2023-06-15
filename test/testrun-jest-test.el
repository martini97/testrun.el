;;; testrun-jest-test.el --- Test for jest and testrun -*- lexical-binding: t -*-

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

(ert-deftest test-testrun-jest-get-test-nearest ()
  "Verify expected test paths with the \"nearest\" scope."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode js-ts-mode
        :language javascript
        :asset "javascriptJest.test.js"
        :position 775
        :body
        (progn
          (should
           (equal
            (testrun-jest-get-test "nearest")
            "test/assets/javascriptJest.test.js -t 'namespace nested namespace this is trickier \\(%#\\)'"))
          (goto-char 372)
          (should
           (equal
            (testrun-jest-get-test "nearest")
            "test/assets/javascriptJest.test.js -t 'namespace root level with only'"))
          (goto-char 53)
          (should
           (equal
            (testrun-jest-get-test "nearest")
            "test/assets/javascriptJest.test.js -t 'root level test'"))))))

(ert-deftest test-testrun-jest-get-test-namespace ()
  "Verify expected test paths with the \"namespace\" scope."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode js-ts-mode
        :language javascript
        :asset "javascriptJest.test.js"
        :position 775
        :body
        (progn
          (should
           (equal
            (testrun-jest-get-test "namespace")
            "test/assets/javascriptJest.test.js -t 'namespace nested namespace'"))
          (goto-char 372)
          (should
           (equal
            (testrun-jest-get-test "namespace")
            "test/assets/javascriptJest.test.js -t 'namespace'"))
          (goto-char 53)
          (should
           (equal
            (testrun-jest-get-test "namespace")
            "test/assets/javascriptJest.test.js"))))))

(ert-deftest test-testrun-jest-get-test-file ()
  "Verify expected test paths with the \"file\" scope."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode js-ts-mode
        :language javascript
        :asset "javascriptJest.test.js"
        :position 775
        :body
        (progn
          (should
           (equal
            (testrun-jest-get-test "file") "test/assets/javascriptJest.test.js"))
          (goto-char 372)
          (should
           (equal
            (testrun-jest-get-test "file") "test/assets/javascriptJest.test.js"))
          (goto-char 53)
          (should
           (equal
            (testrun-jest-get-test "file") "test/assets/javascriptJest.test.js"))))))

(ert-deftest test-testrun-jest-get-test-all ()
  "Verify expected test paths with the \"all\" scope."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode js-ts-mode
        :language javascript
        :asset "javascriptJest.test.js"
        :position 775
        :body
        (progn
          (should (equal (testrun-jest-get-test "all") ""))
          (goto-char 372)
          (should (equal (testrun-jest-get-test "all") ""))
          (goto-char 53)
          (should (equal (testrun-jest-get-test "all") ""))))))

(ert-deftest test-testrun-jest-run ()
  "Sort of an integration test for the compile command of jest."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory)))
            ((symbol-function 'file-executable-p)
             (lambda (_f) nil))
            ((symbol-function 'compile)
             (lambda (cmd commint)
               (should (equal cmd "jest test/assets/javascriptJest.test.js -t 'root level test'"))
               (should commint))))
    (test-testrun-treesit-setup
     :mode js-ts-mode
     :language javascript
     :asset "javascriptJest.test.js"
     :position 53
     :body
     (testrun-nearest)))

  ;; running with local jest
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory)))
            ((symbol-function 'file-executable-p)
             (lambda (_f) t))
            ((symbol-function 'compile)
             (lambda (cmd commint)
               (should
                (equal
                 cmd
                 (concat
                  (expand-file-name "node_modules/.bin/jest" default-directory)
                  " test/assets/javascriptJest.test.js -t 'root level test'")))
               (should commint))))
    (test-testrun-treesit-setup
     :mode js-ts-mode
     :language javascript
     :asset "javascriptJest.test.js"
     :position 53
     :body
     (testrun-nearest)))

  ;; running with local yarn test
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory)))
            ((symbol-function 'file-executable-p)
             (lambda (_f) t))
            ((symbol-function 'compile)
             (lambda (cmd commint)
               (should
                (equal
                 cmd "yarn test test/assets/javascriptJest.test.js -t 'root level test'"))
               (should commint))))
    (let ((testrun-runners '((jest . ("yarn" "test")))))
      (test-testrun-treesit-setup
       :mode js-ts-mode
       :language javascript
       :asset "javascriptJest.test.js"
       :position 53
       :body
       (testrun-nearest))))
  )

(provide 'testrun-jest-test)
;;; testrun-jest-test.el ends here
