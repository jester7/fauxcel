(ns fauxcel.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [fauxcel.base.state :as state :refer [current-cell-sheet]]
   [fauxcel.components.toolbar.toolbar :refer [toolbar]]
   [fauxcel.components.spreadsheet :refer [cellgrid]]
   [fauxcel.components.tabs :refer [cell-sheet-footer]]
   [fauxcel.util.dom :as dom]))

;; -------------------------
;; Views

(defn home-page []
  [:div.appwrapper
   [toolbar]
   [:div#lispy]
   [cellgrid]
   [cell-sheet-footer]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (r/after-render #(dom/draggable (dom/query-selector "#lispy") (dom/query-selector "html")))
  (mount-root))
