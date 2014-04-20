(ns magepunch.engine.db.tasks
  (:gen-class)
  (:require [datomic.api :as d]
            [com.flyingmachine.datomic-junk :as dj]
            [clojure.java.io :as io]
            [flyingmachine.webutils.utils :refer :all]
            [magepunch.engine.db.schema :as schema]))

(defn create
  []
  (d/create-database dj/*db-uri*))

(defn delete
  []
  (d/delete-database dj/*db-uri*))

(defn recreate
  []
  (delete)
  (create))

(def schema-names
  [:base])

(def schema-attr :magepunch/schema)

(defn schema-path
  [schema-name]
  (str "schemas/" (name schema-name) ".edn"))

(defn schema-data
  [schema-name]
  {:txes [(-> schema-name
              schema-path
              read-resource)]})

(defn schema-map
  [schema-names]
  (reduce (fn [m name]
            (assoc m name (schema-data name)))
          {}
          schema-names))

(defnpd install-schemas
  [[schemas schema-names]]
  (apply schema/ensure-schemas
         (into [(dj/conn) schema-attr (schema-map schemas)] schemas)))

(defn rename-schemas
  [name-map]
  (let [schemas (dj/all schema-attr)]
    (filter identity (map (fn [schema]
                            (if-let [new-name (get name-map (get schema schema-attr))]
                              [:db/add (:db/id schema) schema-attr new-name]))
                          schemas))))

(defn seed
  []
  (dj/t (read-resource "fixtures/seeds.edn")))

(defnpd reload
  [[schemas schema-names]]
  (recreate)
  (install-schemas schemas))
