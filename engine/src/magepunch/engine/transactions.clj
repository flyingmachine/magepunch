(ns magepunch.engine.transactions
  (:require [datomic.api :as d]))

(defn tid
  []
  (d/tempid :db.part/user))

(defn dbid
  [x]
  (merge {:db/id (tid)} x))

(def new-user #(dbid {:user/screenname %}))

(defn new-health
  [user match hp]
  (dbid {:health/magepuncher user
         :health/match match
         :health/hp hp}))

(def new-ent
  {:from   new-user
   :target new-user
   :match (fn [users num]
            (dbid {:match/magepunchers users
                   :match/num num}))
   :round (fn [match num]
            (dbid {:round/match match
                   :round/num num}))
   :move  (fn [round user sequence]
            (dbid {:move/round round
                   :move/sequence (clojure.string/join " " sequence)
                   :move/magepuncher user}))
   :health new-health})
