(describe "blackjack-card"
          (it "has a default id"
              (expect (slot-value card 'id) :to-be 0)))

(describe "blackjack--deal-new-hand"
          (after-each
           (setq player-hands (slot-value game 'player-hands))))

(root level sexp)
