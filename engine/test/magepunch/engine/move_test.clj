(ns magepunch.engine.move-test
  (:require [magepunch.engine.move :as move])
  (:use midje.sweet))

(def test-dm
  {:sender {:screen_name "bigpunch"}
   :text "@tinyknuckles p p c"})

(fact "DMs are parsed nicely"
  (move/parse-dm test-dm)
  => {:from "bigpunch"
      :moves ["p" "p" "c"]
      :target "tinyknuckles"})


