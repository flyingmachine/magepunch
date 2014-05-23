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
  (apply map + (map #(get pair-damages [%1 %2]) p1 p2)))

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
(def submission-process-tracking
  {:flags {}
   :refs {:user #{}}
   :transactions []})

(defn add-transaction
  "A submission can include an indeterminate number of transaction"
  [tracking transaction]
  (update-in tracking [:transactions] #(conj % transaction)))

(defn add-flag
  "flags help keep track of what entities don't exist yet"
  [tracking key]
  (assoc-in tracking [:flags key] true))

(defn flag
  [tracking key]
  (get-in tracking [:flags key]))

(defn ffind
  [pred col]
  (first (find pred col)))

(defn track-id
  "track refs, whether for entities-to-be or existing ones"
  [tracking type ent]
  (let [id (:db/id ent)]
    (if (coll? (get-in tracking [:refs type]))
      (update-in tracking [:refs type] #(conj % id))
      (assoc-in tracking [:refs type] id))))

(defn series-num
  "keep track of which x this is, e.g. match 1, match 2, match 3, or
  round 1, round 2, etc"
  [series num-field]
  (if (empty? series)
    1
    (inc (apply max (map num-field series)))))

;; TODO consider making this a function
;; only downside is unnecessarily calling new-ent fns, but those are
;; cheap anywawy
(defmacro track-ent
  [tracking-name ent-kw existing-ent new-ent]
  `(if-let [ent# ~existing-ent]
     (track-id ~tracking-name ~ent-kw ent#)
     (let [ent# ~new-ent]
       (-> ~tracking-name
           (track-id ~ent-kw ent#)
           (add-transaction ent#)
           (add-flag ~(keyword (str "new-" (name ent-kw))))))))

(defmacro current
  [col pred]
  `(if (empty? ~col)
     nil
     (ffind ~pred ~col)))

(defn users
  "look up users, create if nonexistent, and add to tracking"
  [_tracking submission]
  (reduce (fn [tracking screenname]
            (track-ent tracking
                       :user
                       (dj/one [:user/screenname screenname])
                       (t/new-user screenname)))
          _tracking
          [(:from submission) (:target submission)]))

(defn find-matches
  [tracking]
  (if (flag tracking :new-user)
    []
    (apply dj/all (map #(vector :match/magepunchers %) users))))

(defn current-match
  [matches]
  (current matches #(nil? (:match/winner %))))

(defn match
  "find current match, create if nonexistent, add to tracking"
  [tracking]
  (let [matches (find-matches tracking)]
    (track-ent tracking
               :match
               (current-match matches)
               (let [users (get-in tracking [:refs :user])]
                 (t/new-match users (series-num matches :match/num))))))

(defn find-rounds
  [tracking]
  (if (flag tracking :new-match)
    []
    (dj/all [:round/match (get-in tracking [:refs :match])])))

(defn current-round
  [rounds]
  (current rounds #(< (count (:move/_round %)) 2)))

(defn round
  [tracking]
  (let [rounds (find-rounds tracking)
        match (get-in tracking [:refs :match])]
    (track-ent tracking
               :round
               (current-round rounds)
               (t/new-round match (series-num rounds :round/num)))))

(defn process-valid-submission!
  [submission]
  (-> (users submission-process-tracking submission)
      match
      round)
  
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

(defn process-invalid-submission!
  [submission])

(defn submit-moves!
  "Reads a DM, parses it, validates it, records result, tweets result"
  [dm]
  (let [submission (dm->submission dm)]
    (if-not (validate-submission submission)
      (process-valid-submission! submission)
      (process-invalid-submission! submission))))
