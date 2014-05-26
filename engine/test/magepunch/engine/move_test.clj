(ns magepunch.engine.move-test
  (:require [magepunch.engine.move :as m]
            [com.flyingmachine.datomic-junk :as dj])
  (:use midje.sweet
        magepunch.engine.test.db-helpers))

(setup-db-background)

(def test-from "bigpunch")
(def test-target "tinyknuckles")
(def test-dm
  {:sender {:screen_name test-from}
   :text (str "@" test-target " p p c")})

(defn users
  []
  (-> (m/dm->submission test-dm)
      m/submission-process-tracking
      m/from
      m/target
      m/users))

(fact "DMs are parsed nicely"
  (m/dm->submission test-dm)
  => {:from "bigpunch"
      :moves ["p" "p" "c"]
      :target "tinyknuckles"})


(fact "You can validate submissions"
  (m/validate-submission (m/dm->submission test-dm))
  => false

  (m/validate-submission {:sender "bigpunch"
                          :target ""
                          :moves ["x"]})
  => {:from   ["this DM is from nobody"],
      :moves  ["please specify three moves"
               "please use 'p' 'z' 'c' or 'h' for moves"]
      :target ["please specify a target, like @opponent"]})

(fact "processing two new users"
  (let [tracking (users)]
    (fact "the new user flags are set"
      (:flags tracking)
      => {:from true :target true :user true})
    (fact "there are two users in the refs"
      (count (m/tref tracking :user))
      => 2)
    (fact "there's a from ref"
      (m/tref tracking :from)
      => truthy)
    (fact "there's a target ref"
      (m/tref tracking :target)
      => truthy)
    (fact "there are two transactions"
      (count (:transactions tracking))
      => 2)))

;; TODO ensure that find-matches finds an existing match
(fact "when processing match"
  (let [tracking (m/match (users))]
    (fact "the new match flag is set"
      (get-in tracking [:flags :match])
      => true)
    (fact "a transaction was added"
      (count (:transactions tracking))
      => 3)
    (let [match (last (:transactions tracking))]
      (fact "the match num is 1"
        (:match/num match)
        => 1))))

;; TODO test finding an existing round
(fact "when processing round"
  (let [tracking (m/round (m/match (users)))]
    (fact "the new round flag is set"
      (get-in tracking [:flags :round])
      => true)
    (fact "a transaction was added"
      (count (:transactions tracking))
      => 4)
    (let [round (last (:transactions tracking))]
      (fact "the round num is 1"
        (:round/num round)
        => 1))))

(fact "when processing move"
  (let [tracking (m/move (m/round (m/match (users))))]
    (fact "a transaction was added"
      (count (:transactions tracking))
      => 5)
    (let [move (last (:transactions tracking))]
      (fact "the user is the same as from"
        (:move/magepuncher move)
        => (m/tref tracking :from)
        => true)
      (fact "move round refers to round"
        (:move/round move)
        => (m/tref tracking :round)
        => true)
      (fact "sequence is correct"
        (:move/sequence move)
        => "p p c"))))

(fact "processing a valid move results in ents getting creating"
  (m/process-valid-submission! (m/dm->submission test-dm))
  (:user/screenname (dj/one [:user/screenname test-from]))
  => test-from)
