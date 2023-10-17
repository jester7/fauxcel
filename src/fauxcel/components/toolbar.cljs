(ns fauxcel.components.toolbar
  (:require
   [fauxcel.base.state :as state :refer [current-formula]]))

(defn toolbar []
  [:ul.toolbar.dark-bg
   [:li.toolbar-item
    [:span.app-name "fauXcel"]]
   [:li.toolbar-item.formula-input
    [:span.toolbar-label "f(x)"]
    [:input.formula-input {:value @current-formula
                           ::on-change
                           (fn [e]
                             (reset! current-formula (-> e .-target .-value)))}]]
   [:li.toolbar-item
    [:a {:href "#"} "About"]]
   [:li.toolbar-item
    [:a {:href "#"} "Contact"]]])