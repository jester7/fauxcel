(ns fauxcel.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [fauxcel.components.toolbar :refer [toolbar]]
   [fauxcel.components.spreadsheet :refer [cellgrid]]))

;; -------------------------
;; Views

(defn home-page []
  [:div.wrapper
   [toolbar]
   [cellgrid]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
