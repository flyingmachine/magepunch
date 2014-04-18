(ns magepunch.engine.score)

(defn symmetrize
  [x]
  (reduce (fn [z [key value]] (assoc z (reverse key) (reverse value)))
          x
          x))

(def combo-damages (symmetrize {["p" "p"] [0  0]
                                ["p" "z"] [30 20]
                                ["p" "c"] [0  25]
                                ["z" "z"] [30 30]
                                ["z" "c"] [60 0]
                                ["c" "c"] [10 10]}))

(defn round-damage
  [p1 p2]
  (apply map + (map #(get damages [%1 %2]) p1 p2)))
