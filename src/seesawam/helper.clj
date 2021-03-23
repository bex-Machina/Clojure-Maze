(ns seesawam.helper
  (:require [seesawam.BackTracker :as BackTrack]
            [seesawam.sidewinder :as SideWinder]
            [seesawam.Print-As-Text :as pat]
            [seesawam.solver :as solver]))

(defn getTime []
  (str "Mazes/"(.format
                 (java.text.SimpleDateFormat. "MM-dd-yyyy'-'HH-mm-ss")
                 (new java.util.Date))".txt"))

(defn SaveMaze [maze]
  (let [filePath (getTime)]
    (spit filePath maze)
    (pat/print-as-text (into [] (reverse maze)))))



(defn getMazeList []
  (let [
        file (clojure.java.io/file "Mazes/")
        fileList (rest (file-seq file))]
    fileList
    ))

(defn solveMaze [selected row col]
  (let [maze (read-string (slurp selected)) solved (solver/run maze 0 0 (- (count maze) 1) (- (count (maze 0)) 1))]
    (into [] (reverse solved))))

