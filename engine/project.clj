(defproject magepunch.engine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.datomic/datomic-free "0.9.4497"]
                 [com.flyingmachine/datomic-junk "0.1.4"]
                 [twitter-api "0.7.5"]
                 [com.flyingmachine/webutils "0.1.6"]]
  :main ^:skip-aot magepunch.engine
  :target-path "target/%s"
  :plugins [[lein-environ "0.4.0"]]
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["dev"]
                   :dependencies [[midje "1.5.0"]]
                   :env {:datomic {:db-uri "datomic:free://localhost:4334/magepunch"
                                   :test-uri "datomic:mem://magepunch"
                                   :schema-attr :magepunch/schema}}}})
