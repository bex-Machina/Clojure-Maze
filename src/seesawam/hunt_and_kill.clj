(ns seesawam.hunt-and-kill
  (:require [seesawam.BackTracker :as BackTrack]))

(defn make-a-row [columns]
  (loop [count 0 row []]
    (if (= columns count)
      row
      (recur (inc count) (conj row {:north true :east true :south true :west true :visited false :searched false :dot false })))))


(defn make-a-grid [rows columns]
  (loop [count 0 grid []]
    (if (= rows count)
      grid
      (recur (inc count) (conj grid (make-a-row columns))))))

(defn north-of [[row col]] [(dec row) col])
(defn south-of [[row col]] [(inc row) col])
(defn west-of  [[row col]] [row (dec col)])
(defn east-of  [[row col]] [row (inc col)])

(defn validPos [j i row cols]
  (if (or (< j 0 ) (< i 0) (> j (- row 1)) (> i (- cols 1)))
    false
    true))


(defn neighborhood [maze curr sr sc]
  (let [ neighbors [] j (curr 0) i (curr 1) n (north-of [j i]) s (south-of [j i]) w (west-of [j i]) e (east-of [j i])]
    (remove nil?(-> neighbors
                    (conj
                      ;north
                      (if (validPos (n 0) (n 1) sr sc )
                        (if (=((nth (nth maze (n 0)) (n 1)):visited) true)
                          [(n 0) (n 1)]
                          ))
                      ;south
                      (if (validPos (s 0) (s 1) sr sc )
                        (if (=((nth (nth maze (s 0)) (s 1)):visited) true)
                          [(s 0) (s 1)]
                          ))
                      ;west
                      (if (validPos (w 0) (w 1) sr sc )
                        (if (=((nth (nth maze (w 0)) (w 1)):visited) true)
                          [(w 0) (w 1)]
                          ))
                      ;east
                      (if (validPos (e 0) (e 1) sr sc )
                        (if (=((nth (nth maze (e 0)) (e 1)):visited) true)
                          [(e 0) (e 1)]
                          ))
                      )))))



(def temp  (make-a-grid 4 4))

(defn NotEmpty [x] (if (empty? x) false true))

(defn walk-a-row [grid currRow rows cols]
  (loop [maze grid currCol 0 item []]
    (if (or (= currCol cols) (NotEmpty item))
      item
      (let [neighborhood (neighborhood maze [currRow currCol] rows cols )
            visited ((nth (nth maze currRow) currCol):visited)]
        (if (and (NotEmpty neighborhood) (= visited false))
          (recur maze (inc currCol) [[currRow currCol] (rand-nth neighborhood)])
          (recur maze (inc currCol) []))))))

(walk-a-row temp 0 4 4)

(defn and-kill [maze rows cols]
  (loop [maze maze currRow 0 item []]
    (if (or (= currRow rows) (NotEmpty item))
      item
      (recur maze (inc currRow) (walk-a-row maze currRow rows cols)))))

(and-kill temp 4 4)


(defn hunt [row col]
  (loop [maze (assoc-in (make-a-grid row col) [0 0 :visited] true) curr[0 0]]
    (if (empty? (and-kill maze row col))
      maze
      (let [neighborhood (BackTrack/neighborhood maze curr row col )]
        (if (empty? neighborhood)
          (let [items (and-kill maze row col)]
            (recur (BackTrack/remove-border maze (first items) (second items)) (second items)))
          (let [next (rand-nth neighborhood)]
            (recur
              (BackTrack/remove-border maze curr next)
              next)))))))
(hunt 4 4)