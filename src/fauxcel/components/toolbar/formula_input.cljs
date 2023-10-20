(ns fauxcel.components.toolbar.formula-input
  (:require
   [fauxcel.base.state :as state :refer [current-formula]]))

(defn formula-input []
  [:li.toolbar-item.formula-input
   [:span.toolbar-label "f(x)"]
   [:input.formula-input {:value @current-formula
                          :on-change
                          (fn [e]
                            (reset! current-formula (-> e .-target .-value)))}]])