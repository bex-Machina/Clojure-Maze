(ns untitled.MazeSolver
  (:require [untitled.BackTrack :as BackTrack ]))


(defn north-of [[row col]] [(dec row) col])
(defn south-of [[row col]] [(inc row) col])
(defn west-of  [[row col]] [row (dec col)])
(defn east-of  [[row col]] [row (inc col)])

(defn validPos [j i row cols]
  (if (or (< j 0 ) (< i 0) (> j (- row 1)) (> i (- cols 1)))
    false
    true))

(def temp (assoc-in (BackTrack/carve-passages 4 4 0 0) [0 0 :searched] true))

(defn neighborhood [maze curr sr sc]
  (let [ neighbors [] j (curr 0) i (curr 1) n (north-of [j i]) s (south-of [j i]) w (west-of [j i]) e (east-of [j i])]
    (remove nil?(-> neighbors
                    (conj
                      ;north
                      (if (validPos (n 0) (n 1) sr sc )
                        (if (and (=((nth (nth maze (n 0)) (n 1)):north) false) (=((nth (nth maze (n 0)) (n 1)):searched) false))
                          [(n 0) (n 1)]
                          ))
                      ;south
                      (if (validPos (s 0) (s 1) sr sc )
                        (if (and (=((nth (nth maze (s 0)) (s 1)):south) false) (=((nth (nth maze (s 0)) (s 1)):searched) false))
                          [(s 0) (s 1)]
                          ))
                      ;west
                      (if (validPos (w 0) (w 1) sr sc )
                        (if (and (=((nth (nth maze (w 0)) (w 1)):west) false) (=((nth (nth maze (w 0)) (w 1)):searched) false))
                          [(w 0) (w 1)]
                          ))
                      ;east
                      (if (validPos (e 0) (e 1) sr sc )
                        (if (and (=((nth (nth maze (e 0)) (e 1)):east) false) (=((nth (nth maze (e 0)) (e 1)):searched) false))
                          [(e 0) (e 1)]
                          ))
                      )))))

(neighborhood temp [0 1] 4 4)


(defn solver [Maze start end]
  (loop [maze (assoc-in Maze [0 0 :searched] true) stack [start] ]
    (if (=(peek stack) end)
      stack
      (let [validpaths (neighborhood maze (peek stack) (count maze) (count (maze 0)))]
        (if (empty? validpaths)
          (recur maze (pop stack))
          (let [next (rand-nth validpaths)]
            (recur (assoc-in Maze [(next 0) (next 1) :searched] true) (conj stack next))))))))

(defn run [row col j i k l]
  (let [maze (BackTrack/carve-passages row col j i)]
    (solver maze [j i] [k l])
    ))


(run 4 4 0 0 0 1)


;:searched



