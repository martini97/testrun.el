;;; testrun-buttercup-test.el --- Test for buttercup -*- lexical-binding: t -*-

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

;;; Code:

(ert-deftest test-testrun-buttercup--beginning-of-thing-at-point-p ()
  "Tests for `testrun-buttercup--beginning-of-thing-at-point-p'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (dolist (test-data '((7023 nil nil)
                         (7012 nil t)
                         (7000 t t)))
      (let ((pos (car test-data))
            (bolp (cadr test-data))
            (bosp (caddr test-data)))
        (goto-char pos)
        (should (equal (testrun-buttercup--beginning-of-thing-at-point-p 'list)
                       bolp))
        (should (equal (testrun-buttercup--beginning-of-thing-at-point-p 'sexp)
                       bosp))))))

(ert-deftest test-testrun-buttercup--get-list-at-point ()
  "Tests for `testrun-buttercup--get-list-at-point'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (dolist (test-data '((7023 (blackjack-game :deck-type 'jacks))
                         (7011 (blackjack-game :deck-type 'jacks))
                         (7000 (setq game (blackjack-game :deck-type 'jacks)))))
      (let ((pos (car test-data))
            (list-at-point (cadr test-data)))
        (goto-char pos)
        (should (equal (testrun-buttercup--get-list-at-point)
                       list-at-point))))))

(ert-deftest test-testrun-buttercup--sexp-parents ()
  "Tests for `testrun-buttercup--sexp-parents'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (should (equal (testrun-buttercup--sexp-parents)
                   '((describe "blackjack--deal-new-hand"
                               (after-each
                                (setq player-hands (slot-value game 'player-hands))
                                (expect (length player-hands) :to-be 1)
                                (setq player-hand (nth 0 player-hands))
                                (expect (length (slot-value player-hand 'cards)) :to-be 2)
                                (setq dealer-hand (slot-value game 'dealer-hand))
                                (expect (length (slot-value dealer-hand 'cards)) :to-be 2)
                                (setq shoe (slot-value game 'shoe))
                                (expect (length shoe) :to-be 48))

                               (describe "with a deck of jacks"
                                         (it "player has 20, shows hand actions"
                                             (spy-on 'blackjack--ask-hand-action)
                                             (setq game (blackjack-game :deck-type 'jacks))
                                             (blackjack--deal-new-hand game)
                                             (expect (slot-value game 'deck-type) :to-be 'jacks)
                                             (expect (slot-value game 'current-menu) :to-be 'hand)))

                               (describe "with a deck of aces"
                                         (it "dealer upcard is an A, shows insurance actions"
                                             (spy-on 'blackjack--ask-insurance-action)
                                             (setq game (blackjack-game :deck-type 'aces))
                                             (blackjack--deal-new-hand game)
                                             (expect (slot-value game 'deck-type) :to-be 'aces)
                                             (expect (slot-value game 'current-menu) :to-be 'insurance)))

                               (describe "with T, T, A, T"
                                         (it "player has blackjack, shows game actions"
                                             (spy-on 'blackjack--ask-game-action)
                                             (setq game (blackjack-game))
                                             (blackjack--shuffle game '(9 9 0 9) t)
                                             (blackjack--deal-new-hand game)
                                             (expect (slot-value game 'deck-type) :to-be 'regular)
                                             (expect (slot-value game 'current-menu) :to-be 'game))))
                     (describe "with a deck of jacks"
                               (it "player has 20, shows hand actions"
                                   (spy-on 'blackjack--ask-hand-action)
                                   (setq game (blackjack-game :deck-type 'jacks))
                                   (blackjack--deal-new-hand game)
                                   (expect (slot-value game 'deck-type) :to-be 'jacks)
                                   (expect (slot-value game 'current-menu) :to-be 'hand)))
                     (it "player has 20, shows hand actions"
                         (spy-on 'blackjack--ask-hand-action)
                         (setq game (blackjack-game :deck-type 'jacks))
                         (blackjack--deal-new-hand game)
                         (expect (slot-value game 'deck-type) :to-be 'jacks)
                         (expect (slot-value game 'current-menu) :to-be 'hand))
                     (setq game (blackjack-game :deck-type 'jacks))
                     (blackjack-game :deck-type 'jacks))))))

(ert-deftest test-testrun-buttercup--filter-by-keyword ()
  "Tests for `testrun-buttercup--filter-by-keyword'."
  (let ((lists '((describe "foo")
                 (it "bar")
                 (should equal)
                 (expect something))))
    (should (equal (testrun-buttercup--filter-by-keyword lists '(describe it))
                   '((describe "foo")
                     (it "bar"))))
    (should (equal (testrun-buttercup--filter-by-keyword lists '(should something))
                   '((should equal))))))

(ert-deftest test-testrun-buttercup--get-test-pattern ()
  "Tests for `testrun-buttercup--get-test-pattern'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (progn
      (should (equal (testrun-buttercup--get-test-pattern 'nearest)
                     (concat "blackjack--deal-new-hand "
                             "with a deck of jacks "
                             "player has 20, shows hand actions")))
      (should (equal (testrun-buttercup--get-test-pattern 'namespace)
                     (concat "blackjack--deal-new-hand "
                             "with a deck of jacks"))))))

(ert-deftest test-testrun-buttercup-get-test ()
  "Tests for `testrun-buttercup-get-test'."
  (test-testrun-setup
    :mode emacs-lisp-mode
    :asset "buttercup-example.el"
    :position 7023
    :body
    (progn
      (should-error (testrun-buttercup-get-test "file")
                    :type 'user-error)
      (should (equal (testrun-buttercup-get-test "nearest")
                     (concat "--pattern=\"^"
                             "blackjack--deal-new-hand "
                             "with a deck of jacks "
                             "player has 20, shows hand actions"
                             "$\"")))
      (should (equal (testrun-buttercup-get-test "namespace")
                     (concat "--pattern=\""
                             "blackjack--deal-new-hand "
                             "with a deck of jacks"
                             "\"")))
      (should-not (testrun-buttercup-get-test "all")))))

(provide 'testrun-buttercup-test)
;;; testrun-buttercup-test.el ends here
