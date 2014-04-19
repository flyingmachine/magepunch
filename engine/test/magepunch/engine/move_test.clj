(ns magepunch.engine.move-test
  (:require [magepunch.engine.move :as move])
  (:use midje.sweet))

(def test-dm
  {:sender {:screen_name "bigpunch"}
   :text "@tinyknuckles p p c"})

(fact "DMs are parsed nicely"
  (move/dm->submission test-dm)
  => {:from "bigpunch"
      :moves ["p" "p" "c"]
      :target "tinyknuckles"})


(fact "You can validate submissions"
  (move/validate-submission (move/dm->submission test-dm))
  => false

  (move/validate-submission {:sender "bigpunch"
                             :target ""
                             :moves ["x"]})
  => {:from   ["this DM is from nobody"],
      :moves  ["please specify three moves"
               "please use 'p' 'z' 'c' or 'h' for moves"]
      :target ["please specify a target, like @opponent"]})

