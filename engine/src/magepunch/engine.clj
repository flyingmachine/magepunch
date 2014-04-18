(ns magepunch.engine
  (:gen-class))

(defn process-tweets
  [])

(defn process-loop
  []
  (process-signups)
  (process-moves)
  (Thread/sleep 10000))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
