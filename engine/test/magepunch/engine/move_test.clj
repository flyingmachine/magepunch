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
  (fact "processing two new users results in two transactions, 2 refs, new user flag")
  (let [tracking (m/users m/submission-process-tracking (m/dm->submission test-dm))]
    (:flags tracking)
    => {:new-user true}
    (count (get-in tracking [:refs :users]))
    => 2
    (count (:transactions tracking))
    => 2

    (fact "passing to match produceses match stuff"
      (let [match (m/match tracking)]
        match
        => {}))))
