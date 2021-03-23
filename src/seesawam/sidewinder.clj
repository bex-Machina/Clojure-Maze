(ns seesawam.sidewinder)

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

(defn validPos [j i row cols]
  (if (or (< j 0 ) (< i 0) (> j (- row 1)) (> i (- cols 1)))
    false
    true))


(defn north-of [[row col]] [(dec row) col])
(defn south-of [[row col]] [(inc row) col])
(defn west-of  [[row col]] [row (dec col)])
(defn east-of  [[row col]] [row (inc col)])

(def temp (make-a-grid 4 4))

(defn CheckEast [item currRow rows cols]
  (let [j (item 0) i (item 1) e (east-of [j i]) ]
    (cond
      (and (validPos (e 0) (e 1) rows cols) (= currRow 0)) e
      (and (validPos (e 0) (e 1) rows cols)(= (rand-nth [0 1]) 1)) e
      :else
      [])))

(defn carve-North [maze item rows cols currRow]
  (if (> currRow 0)
    (let [j (item 0) i (item 1) n (north-of [j i]) ]
      (if (validPos (n 0) (n 1) rows cols)
        (assoc-in (assoc-in maze [j i :north] false) [(n 0) (n 1) :south] false)))
    maze))

(defn carve-east [maze item rows cols]
  (let [j (item 0) i (item 1) n (east-of [j i]) ]
    (if (validPos (n 0) (n 1) rows cols)
      (assoc-in (assoc-in maze [j i :east] false) [(n 0) (n 1) :west] false))))


(defn walk-a-row [grid currRow rows cols]
  (loop [maze grid  runSet [[currRow 0]] Ncol 0]
    (if (= Ncol (- cols 1))
      (carve-North maze (rand-nth runSet) rows cols currRow)
      (let [next (CheckEast (peek runSet) currRow rows cols)]
        (if (or (empty? next))
          (recur (carve-North maze (rand-nth runSet) rows cols currRow) [[currRow (inc Ncol)]] (inc Ncol))
          (recur (carve-east maze (peek runSet)rows cols) (conj runSet next) (inc Ncol)))))))


(defn sidewinder [rows cols]
  (loop [maze (make-a-grid rows cols) currRow 0]
    (if (= currRow rows)
      maze
      (recur (walk-a-row maze currRow rows cols) (inc currRow)))))

