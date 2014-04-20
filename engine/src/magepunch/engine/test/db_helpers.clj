(ns rabble.test.db-helpers
  (:require [datomic.api :as d]
            [com.flyingmachine.datomic-junk :as dj])
  (:import java.io.File))

;; TODO refactor this out eventually
(def test-db-uri "datomic:mem://magepunch-test")

(defmacro with-test-db
  [& body]
  `(binding [dj/*db-uri* test-db-uri]
     ~@body))
