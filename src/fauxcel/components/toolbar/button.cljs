(ns fauxcel.components.toolbar.button)

(defn button
  ([icon caption]
   [:li.toolbar-item.button
    (when icon
      [:span.icon icon])
    (when caption
      [:span.caption caption])])
  ([icon caption classname]
   [:li.toolbar-item.button {:class classname}
    (when icon
      [:span.icon icon])
    (when caption
      [:span.caption caption])])
  ([icon caption classname use-caption-as-tooltip?]
   [:li.toolbar-item.button {:class classname
                      :title (when use-caption-as-tooltip? caption)}
    (when icon
      [:span.icon icon])]))

