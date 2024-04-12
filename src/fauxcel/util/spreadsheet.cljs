(ns fauxcel.util.spreadsheet
  (:require [fauxcel.base.constants :as c]))

(defn row-col-add
  "Adds specified numbers to row and col cell reference and returns new row-col tuple map"
  [rc-map row-n col-n]
  (let [new-row (+ (:row rc-map) row-n)
        new-col (+ (:col rc-map) col-n)]
    (if (or (< new-row 1)
            (> new-row (dec c/max-rows))
            (< new-col 1)
            (> new-col (dec c/max-cols)))
      rc-map ; return original if out of range
      {:row new-row :col new-col}))) ; else return updated row/col
