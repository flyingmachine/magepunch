(ns magepunch.engine.move
  (:require [clojure.string :as s]
            [flyingmachine.webutils.validation :as v]
            [com.flyingmachine.datomic-junk :as dj]
            [magepunch.engine.parse :as p]
            [magepunch.engine.transactions :as t]))

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
  (apply map + (map #(get pair-damages [%1 %2])
                    (s/split p1 #"\s")
                    (s/split p2 #"\s"))))

(defn tweet-move-result!
  "send a tweek to the two players announcing move result"
  [move-result])

;;;;;;
;; Submission processing
;;;;;;

;; Pipe submission through a series of processors, building up a final
;; submission map which contains datomic transactions

;; Initial tracking map which grows as submission is processed
(defn submission-process-tracking
  [submission]
  {:flags {}
   :refs {:user #{}}
   :ents {}
   :all {}
   :transactions []
   :errors #{}
   :submission submission})

(defn add-transaction
  "A submission can include an indeterminate number of transaction"
  [tracking transaction]
  (update-in tracking [:transactions] conj transaction))

(defn add-flag
  "flags help keep track of what entities don't exist yet"
  [tracking key]
  (assoc-in tracking [:flags key] true))

(defn add-error
  [tracking error]
  (update-in tracking [:errors] conj error))

(defn add-all
  [tracking key val]
  (assoc-in tracking [:all key] val))

(defn tracking-lookup
  [l1]
  (fn [tracking l2] (get-in tracking [l1 l2])))

(def flag (tracking-lookup :flags))
(def tref (tracking-lookup :refs))
(def tent (tracking-lookup :ents))
(def tall (tracking-lookup :all))
(def tsub (tracking-lookup :submission))

(defn ffilter
  [pred col]
  (first (filter pred col)))

(defn add-ref
  "track refs, whether for entities-to-be or existing ones"
  [tracking type ent]
  (assoc-in tracking [:refs type] (:db/id ent)))

(defn assoc-ent
  [tracking id ent]
  (assoc-in tracking [:ents id] ent))

(defn series-num
  "keep track of which x this is, e.g. match 1, match 2, match 3, or
  round 1, round 2, etc"
  [series num-field]
  (if (empty? series)
    1
    (inc (apply max (map num-field series)))))

(defn add-new-ent
  [tracking ent-type new-ent]
  (-> tracking
      (add-ref ent-type new-ent)
      (add-transaction new-ent)
      (add-flag ent-type)))

(defn add-ent
  "add correct tracking for new or existing ents"
  [tracking ent-type existing-ent & new-ent-args]
  (if existing-ent
    (add-ref tracking ent-type existing-ent)
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
                    [:refs :users]
                    (map (partial tref tracking) [:from :target]))]
    (if (or (flag tracking :from) (flag tracking :target))
      (assoc-in t [:flags :users] true)
      t)))

(defn find-ents
  [tracking parent-key parent-ref-key]
  (if (flag tracking parent-ref-key)
    []
    (let [refs (tref tracking parent-ref-key)]
      (if (seq? refs)
        (apply dj/all (map #(vector parent-key %) refs))
        (dj/all [parent-key refs])))))

(defn find-matches
  [tracking]
  (find-ents tracking :match/magepunchers :users))
(def current-match (partial ffilter #(and (nil? (:match/winner %))
                                          (nil? (:match/draw %)))))

(defn add-ent*
  [tracking {:keys [all-finder current-finder ent-key parent-ref-key num-key]}]
  (let [all (all-finder tracking)]
    (-> (add-all tracking ent-key all)
        (add-ent ent-key
                 (current-finder all)
                 (tref tracking parent-ref-key)
                 (series-num all num-key)))))

(defn match
  "find current match, create if nonexistent, add to tracking"
  [tracking]
  (add-ent* tracking
            {:all-finder find-matches
             :current-finder current-match
             :ent-key :match
             :parent-ref-key :users
             :num-key :match/num}))

(defn find-rounds
  [tracking]
  (find-ents tracking :round/match :match))
(def current-round (partial ffilter #(< (count (:move/_round %)) 2)))

(defn round
  "Track current round and all rounds"
  [tracking]
  (add-ent* tracking
            {:all-finder find-rounds
             :current-finder current-round
             :ent-key :round
             :parent-ref-key :match
             :num-key :round/num}))

(defn move
  [tracking]
  (if (or (flag tracking :round)
          (nil? (dj/one [:move/round (tref tracking :round)]
                        [:move/magepuncher (tref tracking :from)])))
    (add-ent tracking
             :move
             nil
             (tref tracking :round)
             (tref tracking :from)
             (tsub tracking :moves))
    (add-error tracking "you've already moved this round")))

(defn health
  "Health values looked up when it's the second move of a round"
  [tracking & players]
  (let [match (tref tracking :match)]
    ;; Only retrieve health values if it's after the first round;
    ;; otherwise create new health ents
    (if (> (count (tall tracking :round)) 1)
      (map (fn [p]
             (let [ent (dj/one [:health/magepuncher p] [:health/match match])
                   hp (:health/hp ent)]
               {:db/id (:db/id ent)
                :health/hp hp
                :health/magepuncher p}))
           players)
      (map #(t/new-health % match 100)
           players))))

(defn second-move?
  "Checks whether current tracking contains second move. If it does,
  return the first move of the round"
  [tracking]
  (and (not (flag tracking :round))
       (dj/one [:move/round (tref tracking :round)])))

(defn damage
  "Add transactions for updating health, if applicable"
  [tracking other-move]
  (let [move (last (:transactions tracking))
        health (health tracking (tref tracking :from) (tref tracking :target))
        damages (round-damage (:move/sequence move) (:move/sequence other-move))]
    (reduce add-transaction
            tracking
            (map (fn [h d]
                   (update-in h [:health/hp] - d))
                 health damages))))

(defn add-draw-transaction
  [tracking]
  (add-transaction tracking {:db/id (tref tracking :match)
                             :match/draw true}))

(defn add-winner-transaction
  [tracking winner]
  (add-transaction tracking {:db/id (tref tracking :match)
                             :match/winner winner}))

(defn draw?
  [hps]
  (every? #(< % 0) hps))

(defn alive?
  [healths]
  (filter #(> (:health/hp %) 0) healths))

(defn winner
  [tracking]
  (let [healths (take-last 2 (:transactions tracking))
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
  [tracking second-move]
  (-> tracking
      (damage second-move)
      winner))

(defn process-invalid-submission!
  [submission errors]
  (println "invalid"))

(defn commit!
  [tracking]
  @(dj/t (:transactions tracking)))

(defn resolve-move!
  [tracking]
  (let [tracking (if-let [other-move (second-move? tracking)]
                   (process-second-move tracking other-move)
                   tracking)]
    (commit! tracking)
    (notify! tracking)))

(defn process-valid-submission!
  [submission]
  (let [tracking (-> (submission-process-tracking submission)
                     from
                     target
                     consolidate-users
                     match
                     round
                     move)]
    (let [errors (:errors tracking)]
      (if (empty? errors)
        (resolve-move! tracking)
        (process-invalid-submission! (:submission tracking) errors)))))

(defn submit-moves!
  "Reads a DM, parses it, validates it, records result, tweets result"
  [dm]
  (let [submission (p/dm->submission dm)]
    (if-let [errors (p/validate-submission submission)]
      (process-invalid-submission! submission errors)
      (process-valid-submission! submission))))
