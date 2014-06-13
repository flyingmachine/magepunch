(ns magepunch.engine.move-test
  (:require [magepunch.engine.move :as m]
            [magepunch.engine.damage :as d]
            [magepunch.engine.parse :as p]
            [com.flyingmachine.datomic-junk :as dj]
            [magepunch.engine.tasks :as t]
            [magepunch.engine.notification :as n]
            [magepunch.engine.tracking :as tr])
  (:use midje.sweet
        magepunch.engine.test.db-helpers))

(setup-db-background)

(def test-from "bigpunch")
(def test-target "tinyknuckles")
(def test-dm
  {:sender {:screen_name test-from}
   :text (str "@" test-target " p p c")})
(def test-dm2
  {:sender {:screen_name test-target}
   :text (str "@" test-from " p z z")})

(def sub1 (p/dm->submission test-dm))
(def sub2 (p/dm->submission test-dm2))

(defn users
  []
  (-> sub1
      tr/submission-process-tracking
      m/from
      m/target
      m/consolidate-users))

(fact "DMs are parsed nicely"
  sub1
  => {:from "bigpunch"
      :moves ["p" "p" "c"]
      :target "tinyknuckles"})


(fact "calculating round damage"
  (d/round-damage "p p c" "p z z")
  => [40 80])

(fact "You can validate submissions"
  (p/validate-submission sub1)
  => false

  (p/validate-submission {:sender "bigpunch"
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
      => {:from true :target true :users true})
    (fact "there are two users in the refs"
      (count (tr/tid tracking :users))
      => 2)
    (fact "there's a from ref"
      (tr/tid tracking :from)
      => truthy)
    (fact "there's a target ref"
      (tr/tid tracking :target)
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
        => (tr/tid tracking :from)
        => true)
      (fact "move round refers to round"
        (:move/round move)
        => (tr/tid tracking :round)
        => true)
      (fact "sequence is correct"
        (:move/sequence move)
        => "p p c"))))

(fact "when processing a completely new valid move"
  (m/process-valid-submission! sub1)
  (let [from   (dj/one [:user/screenname test-from])
        target (dj/one [:user/screenname test-target])
        match  (dj/one :match/num)
        round  (dj/one :round/num)
        move   (dj/one :move/sequence)]

    (fact "users are created"
      (:user/screenname from)
      => test-from
      (:user/screenname target)
      => test-target)

    (fact "a match is created"
      (:match/num match)
      => 1
      (count (:match/magepunchers match))
      => 2
      (:match/winner match)
      => nil)

    (fact "a round is created"
      (:round/num round)
      => 1
      (:round/match round)
      => match)

    (fact "a move is created"
      (:move/sequence move)
      => "p p c"
      (:move/round move)
      => round
      (:move/magepuncher move)
      => from)))

(fact "processing two moves"
  (t/reload!)
  (m/process-valid-submission! sub1)
  (m/process-valid-submission! sub2)

  (let [from    (dj/one [:user/screenname test-from])
        target  (dj/one [:user/screenname test-target])
        match   (dj/one :match/num)
        moves   (dj/all :move/sequence)
        from-health   (dj/one [:health/magepuncher (:db/id from)])
        target-health (dj/one [:health/magepuncher (:db/id target)])]

    (fact "there are two moves"
      (count moves)
      => 2)

    (fact "health is accounted for"
      (:health/hp from-health)
      => 60

      (:health/hp target-health)
      => 20)))

;; You have to wait until the next round to submit your next move
(fact "processing invalid second move"
  (t/reload!)
  (m/process-valid-submission! sub1)
  (m/process-valid-submission! sub1)

  (let [moves (dj/all :move/sequence)]
    (fact "there is one move"
      (count moves)
      => 1)))

(fact "processing entire match"
  (t/reload!)
  (m/process-valid-submission! sub1)
  (m/process-valid-submission! sub2)
  (m/process-valid-submission! sub1)
  (m/process-valid-submission! sub2)

  (let [from    (dj/one [:user/screenname test-from])
        target  (dj/one [:user/screenname test-target])
        match   (dj/one :match/num)
        moves   (dj/all :move/sequence)
        from-health   (dj/one [:health/magepuncher (:db/id from)])
        target-health (dj/one [:health/magepuncher (:db/id target)])]

    (fact "there are four moves"
      (count moves)
      => 4)

    (fact "health is accounted for"
      (:health/hp from-health)
      => 20
      (:health/hp target-health)
      => -60)

    (fact "there is a winner"
      (:match/winner match)
      => from)))

(fact "notifications"
  (t/reload!)
  (m/process-valid-submission! sub1)
  (n/notification (m/track-submission-data sub2))
  => "@tinyknuckles p z z -80 20\n@bigpunch p p c -40 60\nround over!")
