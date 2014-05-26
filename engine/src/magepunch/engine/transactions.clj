(ns magepunch.engine.transactions
  (:require [datomic.api :as d]))

(defn tid
  []
  (d/tempid :user/part))

(defn dbid
  [x]
  (merge {:db/id (tid)} x))

(def new-ent
  {:user #(dbid {:user/screenname %})
   :match (fn [users num] (dbid {:match/magepunchers users :match/num num}))
   :round (fn [match num] (dbid {:round/match match :round/num num}))})
