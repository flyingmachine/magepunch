(ns magepunch.engine.notification
  (:require [clojure.string :as s]
            [magepunch.engine.tracking :refer :all]))
(defn sn
  [screenname]
  (str "@" screenname))

(defn player-move
  [screenname moves damage health]
  (str (sn screenname)
       " " moves
       " " (* -1 damage)
       " " (:health/hp health) "\n"))

(defn move-notification
  [tracking]
  (str
   (player-move (tsub tracking :from)
                (s/join " " (tsub tracking :moves))
                (tent tracking :from-damage)
                (tent tracking :from-health))
   (player-move (tsub tracking :target)
                (:move/sequence (tent tracking :first-move))
                (tent tracking :target-damage)
                (tent tracking :target-health))))

(defn winner-notification
  [tracking]
  (str "@" (:user/screenname (tent tracking :winner)) " wins!"))

(defn draw-notification
  [tracking]
  "It was a draw!")

(defn round-over-notification
  [tracking]
  "round over!")

(defn your-turn-notification
  [tracking]
  (str (sn (tsub tracking :target)) ", "
       (sn (tsub tracking :from)) " has magepunched you!"))

(defn notification
  [tracking]
  (if (first-move-exists? tracking)
    (str (move-notification tracking)
         (cond (tent tracking :winner) (winner-notification tracking)
               (flag tracking :draw) (draw-notification tracking)
               :else (round-over-notification tracking)))
    (your-turn-notification tracking)))
