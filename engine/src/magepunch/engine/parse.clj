(ns magepunch.engine.parse
  "Given a DM, parse it into something pretty"
  (:require [clojure.string :as s]
            [flyingmachine.webutils.validation :as v]))

(defn dm-from
  "who sent the dm"
  [dm]
  (get-in dm [:sender :screen_name]))

(defn dm-target
  "the other player"
  [dm]
  (second (re-find #"@([^ ]+)" (:text dm))))

(defn dm-moves
  "the three moves in the DM, e.g. p c h"
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
