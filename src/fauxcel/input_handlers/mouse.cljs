(ns fauxcel.input-handlers.mouse
  (:require
   [fauxcel.base.state :as state :refer [cells-map edit-mode current-selection
                                         sel-col-offset sel-row-offset current-rc]]
   [fauxcel.base.utility :as base-util :refer [update-selection! update-multi-selection!
                                               row-col-for-cell-ref selection-cell-ref recursive-deref
                                               cell-ref-for-input row-col-for-el curr-selection-is-multi?]]
   [fauxcel.input-handlers.keyboard :as keyboard :refer [get-key-press-info shift-key? not-shift-key?]]
   [fauxcel.util.debug :as debug :refer [debug-log-detailed]]))

(defn reset-selection-offset!
  "Sets the offset of the selection from the current cell"
  [e]
  (let [cell (.-target e)
        rc-clicked-cell (row-col-for-el cell)
        rc-selection (row-col-for-cell-ref (cell-ref-for-input (selection-cell-ref)))]
    (debug-log-detailed "rc-clicked-cell" rc-clicked-cell "rc-selection" rc-selection)
    (when (and (> (:row rc-clicked-cell) 0) (> (:col rc-clicked-cell) 0))
      (reset! sel-row-offset (- (:row rc-clicked-cell) (:row rc-selection)))
      (reset! sel-col-offset (- (:col rc-clicked-cell) (:col rc-selection))))))

(defn click
  "Handles the click event for the spreadsheet cell grid"
  [e]
  (let [key (get-key-press-info e)]
    (when (not-shift-key? key)
      (when (and @edit-mode (not= @current-selection (cell-ref-for-input (.-target e))))
        (when (not (nil? (selection-cell-ref)))
          (reset! edit-mode false)
          (set! (-> (selection-cell-ref) .-value)
                (recursive-deref (:value (@cells-map @current-selection))))
          (set! (-> (selection-cell-ref) .-readOnly) true)))
      (update-selection! (.-target e)))
    (when (shift-key? key)
      ;(if (curr-selection-is-multi?)

       ; )
      (reset-selection-offset! e)
      
      (update-multi-selection! (:row @current-rc) (:col @current-rc) (+ (:row @current-rc) @sel-row-offset) (+ (:col @current-rc) @sel-col-offset)))))

(defn double-click
  "Handles the double click event for the spreadsheet cell grid"
  [e]
  (reset! edit-mode true)
  (update-selection! (.-target e) true)
  (set! (-> e .-target .-readOnly) false))
