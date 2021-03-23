(ns seesawam.core
  (:use [seesaw.core]
        [seesaw.font])
  (:require [seesawam.BackTracker :as BackTrack]
            [seesawam.hunt-and-kill :as hunt-and-kill]
            [seesawam.sidewinder :as side]
            [seesawam.Print-As-Text :as pat]
            [seesawam.helper :as helper]
            [seesawam.solver :as solver]))



(defn slider-panel []
  (let [
        x-slider (slider :id :x-slider :min 2 :max 15 :value 4 :paint-ticks? true :major-tick-spacing 1)
        y-slider (slider :id :y-slider :min 2 :max 15 :value 4 :paint-ticks? true :major-tick-spacing 1)
        x-label (label :text "Columns"
                       :font (font :name :serif :size 10))
        y-label (label :text "Rows"
                       :font (font :name :serif :size 10))
        panel (grid-panel :rows 2 :columns 2 :hgap 2
                          :items [ y-label y-slider x-label x-slider])
        ]
    panel))

(defn matchMaze [func row col]
  (cond
    (= func "BackTrack") (helper/SaveMaze (BackTrack/carve-passages row col))
    (= func "Hunt-and-Kill")  (helper/SaveMaze (hunt-and-kill/hunt row col))
    (= func "sidewinder")  (helper/SaveMaze (side/sidewinder row col))))


(defn control-panel [maze-panel]
  (let [
        combo (combobox  :model ["BackTrack" "Hunt-and-Kill" "sidewinder"] )
        combo2 (combobox  :model (helper/getMazeList))
        generate-btn (button :text "Generate Maze"
                           :font (font :name :sans-serif :size 14))
        solve-brn (button :text "Solve Maze"
                             :font (font :name :sans-serif :size 14))
        slider-panel (slider-panel)
        panel (vertical-panel
                :border 5
                :items [combo slider-panel generate-btn combo2 generate-btn  solve-brn])]
    (listen solve-brn :action
            (fn [e]
              (let [maze (str (selection combo2))
                    columns (value (select slider-panel [:#x-slider]))
                    rows (value (select slider-panel [:#y-slider]))]
                (text! (select maze-panel [:#txt-area]) (pat/print-as-text (helper/solveMaze maze rows columns))))))
    (listen generate-btn :action
            (fn [e]
              (let [columns (value (select slider-panel [:#x-slider]))
                    rows (value (select slider-panel [:#y-slider]))]
                 (text! (select maze-panel [:#txt-area]) (matchMaze (str (selection combo)) rows columns ))
                 (.addItem combo2 (first(reverse(helper/getMazeList))) ))))
    panel))

(defn maze-panel []
  (let [panel (scrollable (text :id :txt-area
                                :text ""
                                :font (font :name :monospaced :size 12)
                                :columns 70
                                :rows 50
                                :multi-line? true
                                :editable? false))]
    panel))

(defn content []
  (let [maze-panel (maze-panel)
        control-panel (control-panel maze-panel)
        panel (border-panel
                :vgap 5
                :west control-panel
                :east  maze-panel)]
    panel))


(defn -main [& args]
  (invoke-later
    (-> (frame :title "Demonstrating SeeSaw",
               :content (content))
        pack!
        show!)))


(-main)

