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
   [button "B" "Bold" "bold" true]
   [button "I" "Italic" "italic" true]
   [button "U" "Underline" "underline" true]
   [button "|" nil "separator" false]
   [button "📂" "Open" "open" true]
   [button "💾" "Save" "save" true]
   [button "📋" "Copy" "copy" true]
   [button "📄" "Paste" "paste" true]
  ;;  [:li.toolbar-item
  ;;   [:a {:href "#"} "About"]]
  ;;  [:li.toolbar-item
  ;;   [:a {:href "#"} "Contact"]]
   ])