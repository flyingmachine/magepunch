#!/usr/bin/env boot

#tailrecursion.boot.core/version "2.3.1"

(set-env!
  :project      'magepunch
  :version      "0.1.0-SNAPSHOT"
  :dependencies '[[tailrecursion/boot.task   "2.1.2"]
                  [tailrecursion/hoplon      "5.7.1"]
                  [org.clojure/clojurescript "0.0-2156"]]
  :out-path     "resources/public"
  :src-paths    #{"src"})

;; Static resources (css, images, etc.):
(add-sync! (get-env :out-path) #{"assets"})

(require '[tailrecursion.hoplon.boot :refer :all])

(deftask development
  "Build magepunch for development."
  []
  (comp (watch) (hoplon {:prerender false :pretty-print true})))

(deftask production
  "Build magepunch for production."
  []
  (hoplon {:optimizations :advanced}))
