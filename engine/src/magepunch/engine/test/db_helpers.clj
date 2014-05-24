(ns magepunch.test.db-helpers
  (:require [datomic.api :as d]
            [com.flyingmachine.datomic-junk :as dj]
            [magepunch.engine.tasks :as t])
  (:use midje.sweet)
  (:import java.io.File))

;; TODO refactor this out eventually
(def test-db-uri (dj/config :test-uri))

(defmacro with-test-db
  [& body]
  `(binding [dj/*db-uri* test-db-uri]
     ~@body))

(defmacro setup-db-background
  [& before]
  `(background
    (before :contents (with-test-db (t/reload) ~@before))
    (around :facts (with-test-db ?form))))
