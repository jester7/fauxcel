(ns fauxcel.components.spreadsheet
  (:require
   [fauxcel.base.state :as state :refer [cells-map]]
   [fauxcel.base.constants :as constants :refer [max-cols max-rows]]
   [fauxcel.base.parser :as parser]
   [fauxcel.base.utility :as base-util :refer [update-selection!]]))

(defn cellgrid []
  [:div.cellgrid.wrapper (base-util/keyboard-navigation)
   (doall (for [row (range 0 max-rows)]
            [:div.row.wrapper {:key (str "row" row)}
             [:span.row-label row]
             (doall (for [col (range 1 max-cols)]
                      (if (= row 0)
                        [:span.col-label {:key (str "col-label-" (char (+ col 64)))} (char (+ col 64))]
                        [:input {:default-value (base-util/recursive-deref (:value (@cells-map (base-util/cell-ref row col))))
                                 :read-only true
                                 :key (str
                                       (base-util/cell-ref row col) "-"
                                       (base-util/recursive-deref (:value (@cells-map (base-util/cell-ref row col)))))
                                 :data-row row :data-col col :id (base-util/cell-ref row col)
                                 :on-change
                                 #(base-util/changed! (.-target %1))
                                 :on-click
                                 #(update-selection! (.-target %1))
                                 :on-double-click
                                 (fn [e]
                                   (update-selection! (.-target e) true)
                                   (set! (-> e .-target .-readOnly) false))
                                 :on-blur
                                 #(base-util/handle-cell-blur (.-target %1) parser/parse-formula)}])))]))])