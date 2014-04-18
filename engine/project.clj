(defproject magepunch.engine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.datomic/datomic-free "0.9.4497"]
                 [com.flyingmachine/datomic-junk "0.1.3"]
                 [twitter-api "0.7.5"]]
  :main ^:skip-aot magepunch.engine
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
