(ns fauxcel.input-handlers.mouse
  (:require
   [fauxcel.base.state :as state :refer [cells-map edit-mode current-selection]]
   [fauxcel.base.utility :as base-util :refer [update-selection! cell-ref col-label cell-in-range?
                                               selection-cell-ref recursive-deref
                                               cell-ref-for-input row-in-range? col-in-range?]]))

(defn click
  "Handles the click event for the spreadsheet cell grid"
  [e]
  (when (and @edit-mode (not= @current-selection (cell-ref-for-input (.-target e))))
    (when (not (nil? (selection-cell-ref)))
      (reset! edit-mode false)
      (set! (-> (selection-cell-ref) .-value)
            (recursive-deref (:value (@cells-map @current-selection))))
      (set! (-> (selection-cell-ref) .-readOnly) true)))
  (update-selection! (.-target e)))

(defn double-click
  "Handles the double click event for the spreadsheet cell grid"
  [e]
  (reset! edit-mode true)
  (update-selection! (.-target e) true)
  (set! (-> e .-target .-readOnly) false))
