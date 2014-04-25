(ns magepunch.engine.db
  (:require [com.flyingmachine.datomic-junk.tasks :as djt]))

(def schema-names [:base])
(def create djt/create)
(def delete djt/delete)
(def recreate djt/recreate)
(def install-schemas (partial djt/install-schemas schema-names))
(def reload (partial djt/reload schema-names))
