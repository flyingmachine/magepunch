(ns magepunch.engine.transactions
  (:require [datomic.api :as d]))

(defn tid
  []
  (d/tempid :user/part))

(defn new-user
  [screenname]
  {:db/id (tid) :user/screenname screenname})

(defn new-match
  [users num]
  {:db/id (tid) :match/magepunchers users :match/num num})
