(defproject songpark/gpio "1.0.0-SNAPSHOT"
  :description "GPIO implementation for Songpark TPX"
  :url ""
  :license {:name ""
            :url ""}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [io.helins/linux.gpio "2.0.1"]]
  :repl-options {:init-ns songpark.gpio})
