(ns fauxcel.components.toolbar.toolbar
  (:require
   [fauxcel.components.toolbar.formula-input :refer [formula-input]]
   [fauxcel.components.toolbar.button :refer [button]]))

(defn toolbar []
  [:ul.toolbar.dark-bg
   [:li.toolbar-item
    [:span.small.logo]]
   [button "|" nil "separator" false]
   [formula-input]
   [button "|" nil "separator" false]
   [button "B" "Bold" "bold disabled" true]
   [button "I" "Italic" "italic disabled" true]
   [button "U" "Underline" "underline disabled" true]
   [button "|" nil "separator" false]
   [button "📂" "Open" "open disabled" true]
   [button "💾" "Save" "save disabled" true]
   [button "📋" "Copy" "copy disabled" true]
   [button "📄" "Paste" "paste disabled" true]
  ;;  [:li.toolbar-item
  ;;   [:a {:href "#"} "About"]]
  ;;  [:li.toolbar-item
  ;;   [:a {:href "#"} "Contact"]]
   ])