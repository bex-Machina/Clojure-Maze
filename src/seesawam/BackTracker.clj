(ns seesawam.BackTracker)
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

(def Temp (assoc-in (make-a-grid 4 4) [0 0 :visited] true))
(assoc-in (make-a-grid 4 4) [0 0 :visited] true)

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
                        (if (=((nth (nth maze (n 0)) (n 1)):visited) false)
                          [(n 0) (n 1)]
                          ))
                      ;south
                      (if (validPos (s 0) (s 1) sr sc )
                        (if (=((nth (nth maze (s 0)) (s 1)):visited) false)
                          [(s 0) (s 1)]
                          ))
                      ;west
                      (if (validPos (w 0) (w 1) sr sc )
                        (if (=((nth (nth maze (w 0)) (w 1)):visited) false)
                          [(w 0) (w 1)]
                          ))
                      ;east
                      (if (validPos (e 0) (e 1) sr sc )
                        (if (=((nth (nth maze (e 0)) (e 1)):visited) false)
                          [(e 0) (e 1)]
                          ))
                      )))))
(neighborhood Temp [2 1] 4 4)


(defn remove-border [maze curr next]
  (let [aj (curr 0) ai (curr 1) bj (next 0) bi (next 1)]
    (cond

      (=(- ai bi )1)  (assoc-in (assoc-in (assoc-in maze [aj ai :west] false) [bj bi :east] false)[bj bi :visited] true)

      (=(- ai bi )-1)  (assoc-in (assoc-in (assoc-in maze [aj ai :east] false) [bj bi :west] false)[bj bi :visited] true)

      (=(- aj bj )1)  (assoc-in (assoc-in (assoc-in maze [aj ai :north] false) [bj bi :south] false)[bj bi :visited] true)

      (=(- aj bj )-1)  (assoc-in (assoc-in (assoc-in maze [aj ai :south] false) [bj bi :north] false)[bj bi :visited] true)
      )))
(remove-border Temp [0 0] [1 0])


(defn carve-passages [row col]
  (loop [maze (assoc-in (make-a-grid row col) [0 0 :visited] true)
         backtrackstack [[0 0]]]
    (if (empty? backtrackstack)
      maze
      (let [neighborhood (neighborhood maze (peek backtrackstack) row col)]
        (if (empty? neighborhood)
          (recur maze (pop backtrackstack))
          (let [next (rand-nth neighborhood)]
            (recur
              (remove-border maze (peek backtrackstack) next)
              (conj backtrackstack next))))))))

