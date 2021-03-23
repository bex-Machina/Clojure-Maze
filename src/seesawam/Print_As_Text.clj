(ns seesawam.Print-As-Text
  (:require [clojure.string :as str]))

(defn print-cell-body [cell]
  (cond
    (and (= false (:east cell)) (= true (:dot cell))) " *  "
    (and (and (= false (:east cell)) (= false (:west cell)) (= false (:south cell))) (= true (:dot cell))) " *  "
    (and (and (= true (:east cell)) (= true (:west cell)) (= true (:south cell)) (= false (:north cell))) (= true (:dot cell))) " * |"
    (and (and (= true (:east cell)) (= false (:south cell))) (= true (:dot cell))) " * |"
    (and (and (= true (:east cell)) (= false (:west cell))) (= true (:dot cell))) " * |"
    (and (= false (:east cell)) (= false (:dot cell))) "    "
    :else
    "   |"))

(defn print-cell-bottom [cell]
  (if (= false (:south cell))
    "   +"
    "---+"))

(defn top-row
  ([c] (top-row c "+"))
  ([c row]

   (if (= 0 c)
     (str row "\n")
     (top-row (dec c) (str row "---+")))))

(defn print-as-text
  [maze]

  (loop [row (- (count maze) 1)
         final (top-row (count (maze 0)))]
    (if (< row 0)
      final
      (recur
        (dec row)
        (str final
             "|"
             (str/join (map #(print-cell-body %) (nth maze row)))
             "\n+"
             (str/join (map #(print-cell-bottom %) (nth maze row)))
             "\n")))))
