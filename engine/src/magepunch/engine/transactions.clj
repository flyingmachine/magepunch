(ns magepunch.engine.transactions
  (:require [datomic.api :as d]))

(defn tid
  []
  (d/tempid :user/part))

(defn dbid
  [x]
  (merge {:db/id (tid)} x))

(def new-user #(dbid {:user/screenname %}))

(def new-ent
  {:from   new-user
   :target new-user
   :match (fn [users num]
            (dbid {:match/magepunchers users :match/num num}))
   :round (fn [match num]
            (dbid {:round/match match :round/num num}))
   :move  (fn [round user sequence]
            (dbid {:move/round round
                   :move/sequence sequence
                   :move/magepuncher user}))})
