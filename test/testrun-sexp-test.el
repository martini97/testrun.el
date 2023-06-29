;;; testrun-sexp-test.el --- Tests for sexp helpers -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alessandro Martini <martini97@protonmail.ch>

;; Author: Alessandro Martini <martini97@protonmail.ch>

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

;; Tests for sexp helpers.

;;; Code:

(require 'cl-macs)
(require 'testrun-sexp)

(ert-deftest test-testrun-sexp--beginning-of-thing-at-point-p ()
  "Tests for `testrun-sexp--beginning-of-thing-at-point-p'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (cl-loop for position in '(7023 7012 7000)
           for beginning-of-list-p in '(nil nil t)
           for beginning-of-sexp-p in '(nil t t)
           do
           (goto-char position)
           (should (equal (testrun-sexp--beginning-of-thing-at-point-p 'list)
                          beginning-of-list-p))
           (should (equal (testrun-sexp--beginning-of-thing-at-point-p 'sexp)
                          beginning-of-sexp-p)))))

(ert-deftest test-testrun-sexp--read-list-at-point ()
  "Tests for `testrun-sexp--read-list-at-point'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (cl-loop for position in '(7023 7011 7000)
             for list-at-point in '((blackjack-game :deck-type 'jacks)
                                    (blackjack-game :deck-type 'jacks)
                                    (setq game (blackjack-game :deck-type 'jacks)))
           do
           (goto-char position)
           (should (equal (testrun-sexp--read-list-at-point)
                          list-at-point)))))

(ert-deftest test-testrun-sexp--parents ()
  "Tests for `testrun-sexp--parents'."
  (test-testrun-setup
   :mode emacs-lisp-mode
   :asset "buttercup-example-small.el"
   :position 7023
   :body
   (cl-loop for position in '(101 195 256)
            for parents in '(((describe "blackjack-card"
                                        (it "has a default id"
                                            (expect (slot-value card 'id) :to-be 0)))
                              (it "has a default id"
                                  (expect (slot-value card 'id) :to-be 0))
                              (expect (slot-value card 'id) :to-be 0)
                              (slot-value card 'id))

                             ((describe "blackjack--deal-new-hand"
                                        (after-each
                                         (setq player-hands (slot-value game 'player-hands))))
                              (after-each
                               (setq player-hands (slot-value game 'player-hands)))
                              (setq player-hands (slot-value game 'player-hands)))

                             ((root level sexp)))
            do
            (goto-char position)
            (should (equal (testrun-sexp--parents) parents)))))

(ert-deftest test-testrun-sexp--filter-car-memq ()
  "Tests for `testrun-sexp--filter-car-memq'."
  (cl-loop for lists in '(((describe "foo") (it "bar") (should equal) (expect something))
                          ((describe "foo") (it "bar") (should equal) (expect something)))
           for wanted in '((describe it)
                           (should something))
           for expected in '(((describe "foo") (it "bar"))
                             ((should equal)))
           do
           (should (equal (testrun-sexp--filter-car-memq lists wanted)
                          expected))))

(provide 'testrun-sexp-test)
;;; testrun-sexp-test.el ends here
