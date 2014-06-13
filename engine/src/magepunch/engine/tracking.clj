(ns magepunch.engine.tracking
  (:require [com.flyingmachine.datomic-junk :as dj]))

(defn submission-process-tracking
  [submission]
  {:flags {}
   :ids {:user #{}}
   :ents {}
   :all {}
   :transactions []
   :errors #{}
   :submission submission})

(defn updater
  [key]
  (fn [tracking x] (update-in tracking [key] conj x)))

(def add-transaction (updater :transactions))
(def add-error (updater :errors))

(defn add-flag
  "flags help keep track of what entities don't exist yet"
  [tracking key]
  (assoc-in tracking [:flags key] true))

(defn add-all
  [tracking key val]
  (assoc-in tracking [:all key] val))

(defn add-id
  "track ids, whether for entities-to-be or existing ones"
  [tracking type ent]
  (assoc-in tracking [:ids type] (:db/id ent)))

(defn assoc-ent
  [tracking key ent]
  (assoc-in tracking [:ents key] ent))

(defn tracking-lookup
  [l1]
  (fn [tracking l2] (get-in tracking [l1 l2])))

(def flag (tracking-lookup :flags))
(def tid (tracking-lookup :ids))
(def tent (tracking-lookup :ents))
(def tall (tracking-lookup :all))
(def tsub (tracking-lookup :submission))

(defn first-move-exists?
  "Checks whether current tracking contains second move. If it does,
  return the first move of the round"
  [tracking]
  (and (not (flag tracking :round))
       (dj/one [:move/round (tid tracking :round)])))
