(ns magepunch.engine.move-test
  (:require [magepunch.engine.move :as m])
  (:use midje.sweet))

(def test-dm
  {:sender {:screen_name "bigpunch"}
   :text "@tinyknuckles p p c"})

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

(facts "about processing users"
  (fact "processing two new users")
  (let [tracking (m/users m/submission-process-tracking (m/dm->submission test-dm))]
    (fact "the new user flag is set"
      (:flags tracking)
      => {:new-user true})
    (fact "there are two users in the refs"
      (count (get-in tracking [:refs :user]))
      => 2)
    (fact "there are two transactions"
      (count (:transactions tracking))
      => 2)

    (fact "when passing to match"
      (let [tracking (m/match tracking)]
        (fact "the new match flag is set"
          (get-in tracking [:flags :new-match])
          => true)
        (fact "a transaction was added"
          (count (:transactions tracking))
          => 3)
        (let [match (last (:transactions tracking))]
          (fact "the match num is 1"
            (:match/num match)
            => 1))))))
