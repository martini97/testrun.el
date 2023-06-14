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

(require 'ert)
(require 'el-mock)

(require 'testrun)
(require 'testrun-core)

(ert-deftest test-testrun-jest ()
  "Test jest integration with testrun."
  (testrun-treesit-test (:language javascript :mode js-ts-mode :content "javascriptJest.test.js" :position 775)
    (with-mock
      (stub buffer-file-name => (expand-file-name
                                 "test/assets/javascriptJest.test.js"
                                 default-directory))
      (stub project-current => (list 'vc 'Git default-directory))
      (mock (compile "jest test/assets/javascriptJest.test.js -t \'namespace nested namespace this is trickier \\(%#\\)\'" t))
      (testrun-nearest)
      (mock (compile "jest test/assets/javascriptJest.test.js -t \'namespace nested namespace\'" t))
      (testrun-namespace)
      (mock (compile "jest test/assets/javascriptJest.test.js" t))
      (testrun-file)
      (mock (compile "jest" t))
      (testrun-all)
      (goto-char 373)
      (mock (compile "jest test/assets/javascriptJest.test.js -t \'namespace root level with only\'" t))
      (testrun-nearest))))

(provide 'testrun-jest-test)
;;; testrun-jest-test.el ends here
