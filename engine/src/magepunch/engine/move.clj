(ns magepunch.engine.move
  (:require [clojure.string :as s]
            [flyingmachine.webutils.validation :as v]
            [com.flyingmachine.datomic-junk :as dj]
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
;; Submission parsing
;;;;;;
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
   :transactions []
   :errors #{}
   :submission submission})

(defn add-transaction
  "A submission can include an indeterminate number of transaction"
  [tracking transaction]
  (update-in tracking [:transactions] #(conj % transaction)))

(defn add-flag
  "flags help keep track of what entities don't exist yet"
  [tracking key]
  (assoc-in tracking [:flags key] true))

(defn add-error
  [tracking error]
  (update-in tracking [:errors] conj error))

(defn flag
  [tracking key]
  (get-in tracking [:flags key]))

(defn tref
  [tracking key]
  (get-in tracking [:refs key]))

(defn ffilter
  [pred col]
  (first (filter pred col)))

(defn track-ref
  "track refs, whether for entities-to-be or existing ones"
  [tracking type ent]
  (assoc-in tracking [:refs type] (:db/id ent)))

(defn series-num
  "keep track of which x this is, e.g. match 1, match 2, match 3, or
  round 1, round 2, etc"
  [series num-field]
  (if (empty? series)
    1
    (inc (apply max (map num-field series)))))

(defn track-new-ent
  [tracking ent-type new-ent]
  (-> tracking
      (track-ref ent-type new-ent)
      (add-transaction new-ent)
      (add-flag ent-type)))

(defn track-ent
  "add correct tracking for new or existing ents"
  [tracking ent-type existing-ent & new-ent-args]
  (if existing-ent
    (track-ref tracking ent-type existing-ent)
    (let [ent (apply (ent-type t/new-ent) new-ent-args)]
      (track-new-ent tracking ent-type ent))))

(defn find-ents
  [tracking parent-key parent-ref-key]
  (if (flag tracking parent-ref-key)
    []
    (let [refs (tref tracking parent-ref-key)]
      (if (seq? refs)
        (apply dj/all (map #(vector parent-key %) refs))
        (dj/all [parent-key refs])))))

(defn user-processor
  [user-key]
  (fn [tracking]
    (let [screenname (get-in tracking [:submission user-key])]
      (track-ent tracking
                 user-key
                 (dj/one [:user/screenname screenname])
                 screenname))))

(def from (user-processor :from))
(def target (user-processor :target))

(defn users
  "consolidate user info for match"
  [tracking]
  (let [t (assoc-in tracking
                    [:refs :users]
                    (map (partial tref tracking) [:from :target]))]
    (if (or (flag tracking :from) (flag tracking :target))
      (assoc-in t [:flags :users] true)
      t)))

(defn find-matches
  [tracking]
  (find-ents tracking :match/magepunchers :users))
(def current-match (partial ffilter #(nil? (:match/winner %))))

(defn match
  "find current match, create if nonexistent, add to tracking"
  [tracking]
  (let [matches (find-matches tracking)]
    (track-ent tracking
               :match
               (current-match matches)
               (tref tracking :users)
               (series-num matches :match/num))))

(defn find-rounds
  [tracking]
  (find-ents tracking :round/match :match))
(def current-round (partial ffilter #(< (count (:move/_round %)) 2)))

(defn round
  "Track current round and all rounds"
  [tracking]
  (let [rounds (find-rounds tracking)
        tracking (track-ref tracking :rounds rounds)]
    (track-ent tracking
               :round
               (current-round rounds)
               (tref tracking :match)
               (series-num rounds :round/num))))

(defn move
  [tracking]
  (if (or (flag tracking :round)
          (nil? (dj/one [:move/round (tref tracking :round)]
                        [:move/magepuncher (tref tracking :from)])))
    (track-ent tracking
               :move
               nil
               (tref tracking :round)
               (tref tracking :from)
               (get-in tracking [:submission :moves]))
    (add-error tracking "you've already moved this round")))

(defn health
  [tracking & players]
  (let [match (tref tracking :match)]
    (if (> (count (tref tracking :rounds)) 0)
      (map #(dj/one [:health/magepuncher %] [:health/match match])
           players)
      (map #((:health t/new-ent) % match 100)
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
            (map #(update-in %1 [:health/hp] - %2) health damages))))

(defn winner
  [tracking]
  tracking)

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
                     users
                     match
                     round
                     move)]
    (let [errors (:errors tracking)]
      (if (empty? errors)
        (resolve-move! tracking)
        (process-invalid-submission! (:submission tracking) errors))))
  
  ;; look up users
  ;; create user if nonexistent
  
  ;; look up match
  ;; if not active match, create new match
  
  ;; look up round
  ;; if no active round, create new round
  
  ;; add move
  ;; if first move of round, notify opponent
  ;; otherwise complete round
  )

(defn submit-moves!
  "Reads a DM, parses it, validates it, records result, tweets result"
  [dm]
  (let [submission (dm->submission dm)]
    (if-let [errors (validate-submission submission)]
      (process-invalid-submission! submission errors)
      (process-valid-submission! submission))))
