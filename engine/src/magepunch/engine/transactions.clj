(ns magepunch.engine.transactions
  (:require [datomic.api :as d]))

(defn tid
  []
  (d/tempid :user/part))

(defn dbid
  [x]
  (merge {:db/id (tid)} x))

(defn new-user
  [screenname]
  (dbid {:user/screenname screenname}))

(defn new-match
  [users num]
  (dbid {:match/magepunchers users :match/num num}))

(defn new-round
  [match num]
  (dbid {:round/match match :match/num num}))
