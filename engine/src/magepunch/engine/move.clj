(ns magepunch.engine.move
  (:require [clojure.string :as s]
            [flyingmachine.webutils.validation :as v]
            [com.flyingmachine.datomic-junk :as dj]
            [magepunch.engine.parse :as p]
            [magepunch.engine.damage :as d]
            [magepunch.engine.notification :as n]
            [magepunch.engine.transactions :as t]
            [magepunch.engine.tracking :refer :all]))

(defn tweet-move-result!
  "send a tweek to the two players announcing move result"
  [move-result])

;;;;;;
;; Submission processing
;;;;;;

;; Pipe submission through a series of processors, building up a final
;; submission map which contains datomic transactions

;; Initial tracking map which grows as submission is processed

(defn ffilter
  [pred col]
  (first (filter pred col)))

(defn series-num
  "keep track of which x this is, e.g. match 1, match 2, match 3, or
  round 1, round 2, etc"
  [series num-field]
  (if (empty? series)
    1
    (inc (apply max (map num-field series)))))

(defn track-ent
  [tracking ent-type ent]
  (-> tracking
      (add-id ent-type ent)
      (assoc-ent ent-type ent)))

(defn add-new-ent
  [tracking ent-type new-ent]
  (-> tracking
      (track-ent ent-type new-ent)
      (add-transaction new-ent)
      (add-flag ent-type)))

(defn add-ent
  "add correct tracking for new or existing ents"
  [tracking ent-type existing-ent & new-ent-args]
  (if existing-ent
    (track-ent tracking ent-type existing-ent)
    (let [ent (apply (ent-type t/new-ent) new-ent-args)]
      (add-new-ent tracking ent-type ent))))

(defn user-processor
  "function factory for from/target processing"
  [user-key]
  (fn [tracking]
    (let [screenname (tsub tracking user-key)]
      (add-ent tracking
               user-key
               (dj/one [:user/screenname screenname])
               screenname))))

(def from (user-processor :from))
(def target (user-processor :target))

(defn consolidate-users
  "consolidate user info for match"
  [tracking]
  (let [t (assoc-in tracking
                    [:ids :users]
                    (map (partial tid tracking) [:from :target]))]
    (if (or (flag tracking :from) (flag tracking :target))
      (assoc-in t [:flags :users] true)
      t)))

(defn find-child-ents
  [tracking parent-key parent-ref-key]
  (if (flag tracking parent-ref-key)
    []
    (let [ids (tid tracking parent-ref-key)]
      (if (seq? ids)
        (apply dj/all (map #(vector parent-key %) ids))
        (dj/all [parent-key ids])))))

(defn add-child
  [tracking {:keys [all-finder current-child-filter ent-key parent-ref-key num-key]}]
  (let [all (all-finder tracking)]
    (-> (add-all tracking ent-key all)
        (add-ent ent-key
                 (current-child-filter all)
                 (tid tracking parent-ref-key)
                 (series-num all num-key)))))

(defn match
  "find current match, create if nonexistent, add to tracking"
  [tracking]
  (add-child tracking
             {:all-finder #(find-child-ents % :match/magepunchers :users)
              :current-child-filter (partial ffilter #(and (nil? (:match/winner %))
                                                           (nil? (:match/draw %))))
              :ent-key :match 
              :parent-ref-key :users
              :num-key :match/num}))

(defn round
  "Track current round and all rounds"
  [tracking]
  (add-child tracking
             {:all-finder #(find-child-ents % :round/match :match)
              :current-child-filter (partial ffilter #(< (count (:move/_round %)) 2))
              :ent-key :round
              :parent-ref-key :match
              :num-key :round/num}))

(defn player-has-moved?
  [tracking]
  (if-let [move (first-move-exists? tracking)]
    (= (:db/id (:move/magepuncher move)) (tid tracking :from))))

(defn move
  [tracking]
  (if-not (player-has-moved? tracking)
    (add-ent tracking
             :move
             nil
             (tid tracking :round)
             (tid tracking :from)
             (tsub tracking :moves))
    (add-error tracking "you've already moved this round")))

(defn retrieve-health
  "Fresh health for a new match"
  [match players]
  (map (fn [p]
         (let [ent (dj/one [:health/magepuncher p] [:health/match match])
               hp (:health/hp ent)]
           {:db/id (:db/id ent)
            :health/hp hp
            :health/magepuncher p}))
       players))

(defn after-first-round?
  [tracking]
  (> (count (tall tracking :round)) 1))

(defn health
  "Health values looked up when it's the second move of a round"
  [tracking & players]
  (let [match (tid tracking :match)]
    (if (after-first-round? tracking)
      (retrieve-health match players)
      (map #(t/new-health % match 100) players))))

(defn assoc-healths
  [tracking healths damages]
  (-> (assoc-ent tracking :from-health (first healths))
      (assoc-ent :target-health (second healths))
      (assoc-ent :from-damage (first damages))
      (assoc-ent :target-damage (second damages))))

(defn damage
  "Add transactions for updating health"
  [tracking other-move]
  (let [move (tent tracking :move)
        health (health tracking (tid tracking :from) (tid tracking :target))
        damages (d/round-damage (:move/sequence move) (:move/sequence other-move))
        updated-healths (map (fn [h d] (update-in h [:health/hp] - d))
                             health damages)
        tracking-with-health (assoc-healths tracking updated-healths damages)]
    (reduce add-transaction
            tracking-with-health
            updated-healths)))

(defn add-draw-transaction
  [tracking]
  (-> (add-flag tracking :draw)
      (add-transaction {:db/id (tid tracking :match)
                        :match/draw true})))

(defn add-winner-transaction
  [tracking winner]
  (-> (assoc-ent tracking :winner winner)
      (add-transaction {:db/id (tid tracking :match)
                        :match/winner winner})))

(defn draw?
  [hps]
  (every? #(< % 0) hps))

(defn alive?
  [healths]
  (filter #(> (:health/hp %) 0) healths))

(defn winner
  [tracking]
  (let [healths [(tent tracking :from-health) (tent tracking :target-health)]
        alive (alive? healths)
        alive-count (count alive)]
    (condp = alive-count
      2 tracking
      1 (add-winner-transaction tracking (:health/magepuncher (first alive)))
      0 (add-draw-transaction tracking))))

(defn notify!
  [tracking]
  )

(defn process-second-move
  [tracking first-move]
  (-> tracking
      (assoc-ent :first-move first-move)
      (damage first-move)
      winner))

(defn process-invalid-submission!
  [submission errors]
  (println "invalid"))

(defn commit!
  [tracking]
  @(dj/t (:transactions tracking)))

(defn resolve-move
  [tracking]
  (if-let [first-move (first-move-exists? tracking)]
    (process-second-move tracking first-move)
    tracking))

(defn track-submission-data
  [submission]
  (let [t (-> (submission-process-tracking submission)
              from
              target
              consolidate-users
              match
              round
              move)]
    (if (empty? (:errors t))
      (resolve-move t)
      t)))

(defn process-valid-submission!
  [submission]
  (let [tracking (track-submission-data submission)
        errors (:errors tracking)]
    (if (empty? errors)
      (do (commit! tracking)
          (notify! tracking))
      (process-invalid-submission! (:submission tracking) errors))))

(defn submit-moves!
  "Reads a DM, parses it, validates it, records result, tweets result"
  [dm]
  (let [submission (p/dm->submission dm)]
    (if-let [errors (p/validate-submission submission)]
      (process-invalid-submission! submission errors)
      (process-valid-submission! submission))))
