(ns adventofcode-2022.core
  (:require [adventofcode-2022.exercises :as exercises])
  (:gen-class))

(defn -main
  "Runs an advent of code exercise"
  [& [exercise part]]
  (exercises/execute exercise part))
