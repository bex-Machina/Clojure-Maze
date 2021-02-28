(ns untitled.core
  (:require [untitled.BackTrack :as BackTrack ]
            [untitled.Print-As-Text :as pat]
            [untitled.MazeSolver :as solver]))


(defn -main [rows cols j i j2 i2]
  (let [maze (BackTrack/carve-passages rows cols j i)]
    (solver/solver (into [] (reverse maze))[j i] [j2 i2])
    ))




(-main 4 4 0 0 3 3)

