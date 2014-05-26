(ns magepunch.engine.tasks
  (require [com.flyingmachine.datomic-junk.tasks :as djt]))

(def schemas [:base])

(def create! djt/create)
(def reload! (partial djt/reload schemas))
