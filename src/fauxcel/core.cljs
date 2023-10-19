(ns fauxcel.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [fauxcel.components.toolbar.toolbar :refer [toolbar]]
   [fauxcel.components.spreadsheet :refer [cellgrid]]
   [fauxcel.util.dom :as dom]))

;; -------------------------
;; Views

(defn home-page []
  [:div.wrapper
   [toolbar]
   [:div#clippy]
   [cellgrid]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (r/after-render #(dom/draggable (dom/querySelector "#clippy") (dom/querySelector "html")))
  (mount-root))
