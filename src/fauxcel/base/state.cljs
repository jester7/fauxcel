(ns fauxcel.base.state
  (:require
   [reagent.core :as r]))

(def current-formula (r/atom ""))
(def cells-map (r/atom {}))
(def current-selection (r/atom ""))
