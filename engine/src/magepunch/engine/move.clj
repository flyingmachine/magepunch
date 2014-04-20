(ns magepunch.engine.move
  (:require [clojure.string :as s]
            [flyingmachine.webutils.validation :as v]
            [com.flyingmachine.datomic-junk :as dj]))

(defn symmetrize
  "Used to avoid repeating the symmetrical value of pair-damages"
  [x]
  (reduce (fn [z [key value]] (assoc z (reverse key) (reverse value)))
          x
          x))

;; - punch does 10 base damage
;; - zap does 30 base damage
;; - counter damages opponent for 2x against zap, receives 2x against
;; punch
;; - heal removes 20 damage
(def pair-damages (symmetrize {["p" "p"] [10  10]
                               ["p" "z"] [30  10]
                               ["p" "c"] [0   20]
                               ["p" "h"] [0   -10]
                               ["z" "z"] [30  30]
                               ["z" "c"] [60  0]
                               ["z" "h"] [0   10]
                               ["c" "c"] [10  10]
                               ["c" "h"] [0   -20]
                               ["h" "h"] [-20 -20]}))

(defn round-damage
  [p1 p2]
  (apply map + (map #(get pair-damages [%1 %2]) p1 p2)))

(defn tweet-move-result
  [move-result])

(defn dm-from
  [dm]
  (get-in dm [:sender :screen_name]))

(defn dm-target
  [dm]
  (second (re-find #"@([^ ]+)" (:text dm))))

(defn dm-moves
  [dm]
  (map str
       (-> (re-find #"@[^\s]+(.*$)" (:text dm))
           second
           (s/replace #"\s+" "")
           seq)))

(defn dm->submission
  [dm]
  {:from   (dm-from dm)
   :target (dm-target dm)
   :moves  (dm-moves dm)})

(def valid-moves #{"p" "z" "c" "h"})
(def submission-validators
  {:from   ["this DM is from nobody"
            #(not-empty %)]
   
   :target ["please specify a target, like @opponent"
            #(not-empty %)]
   
   :moves  ["please specify three moves"
            #(= 3 (count %))

            "please use 'p' 'z' 'c' or 'h' for moves"
            #(every? valid-moves %)]})

(defn validate-submission
  [submission]
  (v/if-valid
   submission submission-validators errors
   false
   errors))

(defn process-valid-submission!
  [submission]
  ())

(defn process-invalid-submission!
  [submission])

(defn submit-moves!
  "Reads a DM, parses it, validates it, records result, tweets result"
  [dm]
  (let [submission (dm->submission dm)]
    (if-not (validate-submission submission)
      (process-valid-submission! submission)
      (process-invalid-submission! submission))))
